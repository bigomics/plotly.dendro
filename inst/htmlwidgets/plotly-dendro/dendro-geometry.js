// Trace Geometry For A Level-of-Detail Cut
//
// Turns a cut (from dendro-peel.js) into the coordinate arrays plotly draws.
// Mirrors the R-side geometry so a dynamic frame is indistinguishable from a
// statically cut plot.
//
// Everything is emitted as NA-separated runs inside a single trace per colour.
// Trace count is the dominant scaling factor in plotly.js, so a few hundred
// clades must not become a few hundred traces.

/**
 * Display x of a merge child. Leaf children (negative hclust encoding) read
 * their position from the leaf table; internal children sit at their node x.
 */
function childX(nodes, leaves, child) {
  return child < 0 ? leaves.x[-child - 1] : nodes.x[child - 1];
}

/**
 * Display y of a merge child. Leaf children sit at whatever y the R geometry
 * assigned them, so `hang` is honoured without recomputing it here.
 */
function childY(nodes, leaves, child) {
  return child < 0 ? leaves.y[-child - 1] : nodes.height[child - 1];
}

/**
 * Branch geometry for the expanded part of the tree.
 *
 * Each expanded merge contributes the classic bracket: up from the left child,
 * across at the merge height, down to the right child. Four vertices, then a
 * gap so the next bracket starts a new polyline.
 *
 * @returns {{x: Array<number>, y: Array<number>}}
 */
function buildSegments(nodes, leaves, expanded) {
  const x = [];
  const y = [];

  for (let k = 0; k < expanded.length; k++) {
    const i = expanded[k];
    const lx = childX(nodes, leaves, nodes.left[i]);
    const rx = childX(nodes, leaves, nodes.right[i]);
    const ly = childY(nodes, leaves, nodes.left[i]);
    const ry = childY(nodes, leaves, nodes.right[i]);
    const h = nodes.height[i];

    if (k > 0) {
      x.push(null);
      y.push(null);
    }
    x.push(lx, lx, rx, rx);
    y.push(ly, h, h, ry);
  }
  return { x: x, y: y };
}

/**
 * Resolve a clade's span and apex height, for either encoding.
 *
 * @param {number} clade - merge index, or negative leaf encoding
 */
function cladeExtent(nodes, leaves, clade) {
  if (clade < 0) {
    const pos = leaves.x[-clade - 1];
    return { lo: pos, hi: pos, height: leaves.y[-clade - 1], members: 1 };
  }
  return {
    lo: nodes.span_lo[clade],
    hi: nodes.span_hi[clade],
    height: nodes.height[clade],
    members: nodes.members[clade]
  };
}

/**
 * Dominant colour of a clade, by majority vote over the leaves it covers.
 *
 * This is the visual summary that a bare leaf-count label cannot give: the
 * glyph says what is inside, not merely how much. Colour semantics belong to
 * the caller -- R ships a palette and a per-leaf palette index, and this only
 * counts them.
 *
 * @returns {number} palette index, or 0 when no colouring was supplied
 */
function dominantColor(leafColor, nPalette, lo, hi) {
  if (!leafColor || nPalette <= 1) {
    return 0;
  }

  const counts = new Int32Array(nPalette);
  for (let p = lo; p <= hi; p++) {
    const c = leafColor[p - 1];
    if (c >= 0 && c < nPalette) {
      counts[c]++;
    }
  }

  let best = 0;
  for (let c = 1; c < nPalette; c++) {
    if (counts[c] > counts[best]) {
      best = c;
    }
  }
  return best;
}

/**
 * Wedge geometry for the collapsed frontier, grouped by palette index.
 *
 * The apex sits at the midpoint of the leaf span, not at the clade's own node
 * position: the latter is truthful but renders unbalanced clades as lopsided
 * right triangles that read as a drawing error.
 *
 * @returns {Array<{x: Array<number>, y: Array<number>}>} one entry per palette slot
 */
function buildClades(nodes, leaves, clades, cfg) {
  const nPalette = Math.max(1, (cfg.palette || []).length);
  const groups = [];
  for (let c = 0; c < nPalette; c++) {
    groups.push({ x: [], y: [] });
  }

  const base = cfg.base;
  const stub = cfg.shape === 'stub';

  for (let k = 0; k < clades.length; k++) {
    const ext = cladeExtent(nodes, leaves, clades[k]);

    // A single leaf gets no glyph. Its branch already terminates at the leaf,
    // so a wedge would be redundant -- and a one-leaf-wide triangle drawn from
    // the floor to its merge height reads as tall as a genuine clade, which is
    // exactly the aggregate-vs-datum confusion the glyph exists to avoid.
    if (ext.members < 2) {
      continue;
    }

    const slot = dominantColor(cfg.leaf_color, nPalette, ext.lo, ext.hi);
    const g = groups[slot];

    if (g.x.length > 0) {
      g.x.push(null);
      g.y.push(null);
    }

    if (stub) {
      const mid = (ext.lo + ext.hi) / 2;
      g.x.push(mid, mid);
      g.y.push(base, ext.height);
    } else {
      const lo = ext.lo - 0.5;
      const hi = ext.hi + 0.5;
      g.x.push(lo, hi, (lo + hi) / 2, lo);
      g.y.push(base, base, ext.height, base);
    }
  }
  return groups;
}

/**
 * Swap/flip coordinates to match a dendrogram orientation, mirroring the R
 * side's .orient_xy().
 */
function orientXY(x, y, orientation) {
  if (orientation === 'bottom') {
    return { x: x, y: y };
  }
  if (orientation === 'top') {
    return { x: x, y: y.map(v => (v === null ? null : -v)) };
  }
  if (orientation === 'left') {
    return { x: y, y: x };
  }
  // right
  return { x: y, y: x.map(v => (v === null ? null : -v)) };
}

/**
 * Thin leaf tick labels to what the axis can legibly show.
 *
 * plotly clips array ticks to the visible range but never thins them, and the
 * scan is O(total tickvals) on every redraw -- including every frame of a pan.
 * So the thinning has to happen here, not in the layout.
 *
 * @returns {{tickvals: Array<number>, ticktext: Array<string>}}
 */
function buildTicks(leaves, window, cfg) {
  const empty = { tickvals: [], ticktext: [] };
  if (!cfg.labels || !cfg.show_labels) {
    return empty;
  }

  const visible = window.span;
  const budget = Math.max(1, Math.floor(cfg.label_budget || 50));
  if (visible > budget * (cfg.label_cutoff_factor || 10)) {
    return empty; // far too dense to be worth drawing at all
  }

  // `p` is a display position, and leaf display positions are the leaf axis's
  // own coordinates, so the tick value is `p` itself. `cfg.labels` is in
  // display order to match -- unlike `leaves`, which is keyed by leaf index.
  const stride = Math.max(1, Math.ceil(visible / budget));
  const tickvals = [];
  const ticktext = [];
  for (let p = window.lo; p <= window.hi; p += stride) {
    tickvals.push(p);
    ticktext.push(cfg.labels[p - 1]);
  }
  return { tickvals: tickvals, ticktext: ticktext };
}
