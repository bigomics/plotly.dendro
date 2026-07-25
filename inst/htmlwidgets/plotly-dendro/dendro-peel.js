// Viewport-Restricted Level-of-Detail Cut
//
// Chooses which merges to draw for the currently visible leaf window, under a
// budget on the number of visible clades. Mirrors R's dendro_cut(), but adds
// viewport restriction so the work per frame is bounded by the budget rather
// than by the size of the tree.
//
// The cut is expressed as a score threshold rather than as a priority-queue
// peel. That is valid because every built-in score is monotone up the tree --
// a parent never scores below either child:
//
//   members  subtree leaf count, monotone by definition
//   height   hclust merge heights are non-decreasing with merge index, and a
//            child always has a lower index than its parent
//   hybrid   height * log1p(members), a product of two monotone factors
//
// With a monotone score, {node : score >= t} is automatically a top-down
// closed set for any t, so no closure bookkeeping is needed. Zooming in only
// lowers t, which can only ever open nodes -- never close them. That is what
// makes the view stable under zoom instead of flickering.
//
// A custom non-monotone score would break this guarantee; the R side refuses
// to attach the dynamic runtime for one.

/**
 * Score every merge under the requested priority.
 *
 * @param {Object} nodes - Column-oriented node table from R
 * @param {string} priority - "members", "height", or "hybrid"
 * @returns {Float64Array} one score per merge
 */
function scoreNodes(nodes, priority) {
  const m = nodes.members.length;
  const out = new Float64Array(m);

  for (let i = 0; i < m; i++) {
    if (priority === 'height') {
      out[i] = nodes.height[i];
    } else if (priority === 'hybrid') {
      out[i] = nodes.height[i] * Math.log1p(nodes.members[i]);
    } else {
      out[i] = nodes.members[i];
    }
  }
  return out;
}

/**
 * Select the threshold that admits at most `budget` clades.
 *
 * Expanding k nodes yields exactly k+1 clades in a binary tree, so the budget
 * on clades is a budget of budget-1 on expansions.
 *
 * @param {Float64Array} scores - all merge scores
 * @param {Int32Array} visible - indices of merges intersecting the viewport
 * @param {number} budget - maximum number of visible clades
 * @returns {number} score threshold; nodes at or above it expand
 */
function pickThreshold(scores, visible, budget) {
  const want = Math.max(1, Math.floor(budget) - 1);
  if (visible.length <= want) {
    return -Infinity; // the whole visible subtree fits; expand all of it
  }

  // Partial selection would be faster asymptotically, but `visible` is already
  // bounded by the tree size and this runs once per settled zoom, not per frame.
  const sorted = Array.prototype.slice.call(visible)
    .map(i => scores[i])
    .sort((a, b) => b - a);
  return sorted[want - 1];
}

/**
 * Compute the level-of-detail cut for a viewport.
 *
 * @param {Object} nodes - node table {x, height, members, span_lo, span_hi, left, right}
 * @param {Float64Array} scores - precomputed scores
 * @param {number} lo - leftmost visible leaf position
 * @param {number} hi - rightmost visible leaf position
 * @param {number} budget - maximum visible clades
 * @param {number} minClade - stop subdividing clades below this leaf count
 * @returns {{expanded: number[], clades: number[]}} merge indices; a clade
 *   index is negative-encoded as -(leafPosition) when the clade is one leaf
 */
function computeCut(nodes, scores, lo, hi, budget, minClade) {
  const m = nodes.members.length;

  // Viewport restriction: a merge matters only if its leaf span overlaps the
  // window. This is what keeps per-frame work independent of tree size.
  const visible = [];
  for (let i = 0; i < m; i++) {
    if (nodes.span_hi[i] >= lo && nodes.span_lo[i] <= hi) {
      if (!(minClade > 1) || nodes.members[i] >= minClade) {
        visible.push(i);
      }
    }
  }

  const threshold = pickThreshold(scores, visible, budget);

  const expandedFlag = new Uint8Array(m);
  const expanded = [];
  for (let k = 0; k < visible.length; k++) {
    const i = visible[k];
    if (scores[i] >= threshold) {
      expandedFlag[i] = 1;
      expanded.push(i);
    }
  }

  // The frontier is every child of an expanded node that is not itself
  // expanded. Leaf children arrive as hclust's negative encoding.
  const clades = [];
  for (let k = 0; k < expanded.length; k++) {
    const i = expanded[k];
    const kids = [nodes.left[i], nodes.right[i]];
    for (let c = 0; c < 2; c++) {
      const child = kids[c];
      if (child < 0) {
        clades.push(child);
      } else if (!expandedFlag[child - 1]) {
        clades.push(child - 1);
      }
    }
  }

  // An unexpanded root means the whole tree is one clade.
  if (expanded.length === 0) {
    clades.push(m - 1);
  }

  return { expanded: expanded, clades: clades, threshold: threshold };
}

/**
 * Map an axis range to an inclusive leaf-position window, clamped to the tree.
 *
 * @param {Array<number>} range - axis range in leaf-position units
 * @param {number} n - leaf count
 * @returns {{lo: number, hi: number, span: number}}
 */
function leafWindow(range, n) {
  let lo = 1;
  let hi = n;

  if (range && range.length === 2 && isFinite(range[0]) && isFinite(range[1])) {
    lo = Math.max(1, Math.floor(Math.min(range[0], range[1])));
    hi = Math.min(n, Math.ceil(Math.max(range[0], range[1])));
  }
  if (hi < lo) {
    hi = lo;
  }
  return { lo: lo, hi: hi, span: hi - lo + 1 };
}

/**
 * Resolve the clade budget for a viewport.
 *
 * In "pixels" mode the budget is derived from how much room each clade would
 * actually get, so the view never draws more detail than the screen can show.
 *
 * @param {Object} cfg - runtime config
 * @param {number} plotPx - width (or height) of the plotting area in pixels
 * @returns {number} clade budget
 */
function resolveBudget(cfg, plotPx) {
  if (cfg.budget_mode !== 'pixels' || !isFinite(plotPx) || plotPx <= 0) {
    return cfg.max_leaves;
  }
  const perLeaf = Math.max(0.5, cfg.min_leaf_px || 3);
  return Math.max(2, Math.min(cfg.max_leaves, Math.floor(plotPx / perLeaf)));
}
