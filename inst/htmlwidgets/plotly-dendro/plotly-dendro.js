// Event Engine and Main Orchestrator
//
// Re-cuts the dendrogram to the visible window whenever the user zooms or
// pans, so detail resolves as they zoom in rather than merely magnifying.
// Exposes window.plotlyDendro for R integration.
//
// LOOP SAFETY -- the single most important rule in this file:
//
//   Plotly.react emits plotly_react. Plotly.restyle emits plotly_restyle.
//   Only Plotly.relayout emits plotly_relayout.
//
// So a handler bound to plotly_relayout that updates via Plotly.react cannot
// re-trigger itself. There is no feedback loop to guard against, provided we
// never call Plotly.relayout here. Tick changes therefore travel inside the
// layout object handed to Plotly.react rather than as a separate relayout
// call. Two further guards are kept as belt-and-braces: a debounce, and an
// idempotence check that returns early when the recomputed window and budget
// would produce the cut that is already on screen.

const DEBOUNCE_MS = 120;

/**
 * Initialize dynamic level-of-detail on a plotly element.
 *
 * @param {HTMLElement} el - Plotly chart element
 * @param {Object} cfg - Configuration from R
 */
function init(el, cfg) {
  // The render hook fires on every Shiny renderValue, so listeners would stack
  // without this. Drop the previous handler before installing a new one.
  if (el._dendro && el._dendro.handler && typeof el.removeListener === 'function') {
    el.removeListener('plotly_relayout', el._dendro.handler);
  }

  const scores = scoreNodes(cfg.nodes, cfg.priority);
  const slots = resolveTraceSlots(el, cfg);

  el._dendro = {
    cfg: cfg,
    scores: scores,
    slots: slots,
    timer: null,
    lastKey: null,
    running: false,
    pending: false,
    handler: null,
    updates: 0
  };

  const handler = function () {
    schedule(el);
  };
  el._dendro.handler = handler;
  el.on('plotly_relayout', handler);

  // Draw the initial level for whatever range the plot opened at.
  apply(el, true);
}

/**
 * Locate the traces this runtime owns, by the tag R wrote into each trace's
 * `meta`. Position-based lookup would be fragile: plotly's own bookkeeping
 * shifts indices, and a caller may add traces either side of ours.
 *
 * @returns {{clades: number[], segments: number}} trace indices, -1 if absent
 */
function resolveTraceSlots(el, cfg) {
  const data = el.data || [];
  const clades = new Array(cfg.n_clade_traces).fill(-1);
  let segments = -1;

  for (let i = 0; i < data.length; i++) {
    const meta = data[i] && data[i].meta;
    if (typeof meta !== 'string' || meta.indexOf(cfg.tag) !== 0) {
      continue;
    }
    const rest = meta.slice(cfg.tag.length);
    if (rest === 'segments') {
      segments = i;
    } else if (rest.indexOf('clade:') === 0) {
      const slot = parseInt(rest.slice('clade:'.length), 10);
      if (slot >= 0 && slot < clades.length) {
        clades[slot] = i;
      }
    }
  }

  if (segments < 0) {
    console.warn('plotly-dendro: could not find its traces; dynamic updates disabled');
  }
  return { clades: clades, segments: segments };
}

/**
 * Debounce re-cuts so a drag does not recompute on every intermediate event.
 */
function schedule(el) {
  const state = el._dendro;
  if (!state) return;

  if (state.timer) {
    clearTimeout(state.timer);
  }
  state.timer = setTimeout(function () {
    state.timer = null;
    apply(el, false);
  }, DEBOUNCE_MS);
}

/**
 * Read the leaf-axis range from the plot itself.
 *
 * Deliberately ignores the relayout event payload: plotly emits only the axes
 * that actually changed, in either `xaxis.range` or `xaxis.range[0]`/`[1]`
 * form, and omits them entirely when a drag ends outside the div. The live
 * layout is the only reliable source.
 */
function leafAxisRange(el, cfg) {
  const fl = el._fullLayout || {};
  const horizontal = cfg.orientation === 'left' || cfg.orientation === 'right';
  const ax = horizontal ? fl.yaxis : fl.xaxis;

  if (!ax || !ax.range) {
    return null;
  }
  // "right" mirrors the leaf axis, so undo that before interpreting positions.
  if (cfg.orientation === 'right') {
    return [-ax.range[1], -ax.range[0]];
  }
  return ax.range.slice();
}

/**
 * Plotting-area extent along the leaf axis, in pixels.
 */
function leafAxisPixels(el, cfg) {
  const fl = el._fullLayout || {};
  const horizontal = cfg.orientation === 'left' || cfg.orientation === 'right';
  const ax = horizontal ? fl.yaxis : fl.xaxis;
  return ax && ax._length ? ax._length : (horizontal ? fl.height : fl.width);
}

/**
 * Recompute the cut for the current viewport and push it, unless it matches
 * what is already drawn.
 *
 * @param {HTMLElement} el - Plotly chart element
 * @param {boolean} force - bypass the idempotence check (initial draw)
 */
function apply(el, force) {
  const state = el._dendro;
  if (!state) return;
  if (typeof Plotly === 'undefined' || !Plotly.react) return;

  // A zoom that lands mid-react must not be dropped, or the user is left
  // looking at the detail level for a window they have already left. Remember
  // that another pass is owed and run it when the in-flight one settles.
  if (state.running) {
    state.pending = true;
    return;
  }

  const cfg = state.cfg;
  const window_ = leafWindow(leafAxisRange(el, cfg), cfg.n);
  const budget = resolveBudget(cfg, leafAxisPixels(el, cfg));

  // Quantize: zooming by a pixel must not trigger a redraw. Only a change in
  // the window or the budget can.
  const key = window_.lo + ':' + window_.hi + ':' + budget;
  if (!force && key === state.lastKey) {
    return;
  }
  state.lastKey = key;
  state.running = true;

  try {
    const cut = computeCut(
      cfg.nodes, state.scores,
      window_.lo, window_.hi, budget, cfg.min_clade
    );

    const data = buildData(el, cfg, state.slots, cut);
    const layout = buildLayout(el, cfg, window_);

    Plotly.react(el, data, layout)
      .then(function () { settle(el); })
      .catch(function (err) {
        console.error('plotly-dendro: react failed', err);
        // Drop the key so the failed level is not mistaken for the drawn one.
        state.lastKey = null;
        settle(el);
      });
    state.updates++;
  } catch (err) {
    console.error('plotly-dendro: cut failed', err);
    state.lastKey = null;
    settle(el);
  }
}

/**
 * Release the in-flight guard and run any pass that arrived while it was held.
 */
function settle(el) {
  const state = el._dendro;
  if (!state) return;

  state.running = false;
  if (state.pending) {
    state.pending = false;
    apply(el, false);
  }
}

/**
 * Build the trace array for a cut.
 *
 * Only the traces this runtime owns are replaced; anything else the caller
 * added is passed through untouched. Trace count is held constant across
 * levels -- unused palette slots become empty arrays rather than disappearing
 * -- because an unequal trace count forces plotly into a full replot.
 */
function buildData(el, cfg, slots, cut) {
  const data = (el.data || []).slice();

  const groups = buildClades(cfg.nodes, cfg.leaves, cut.clades, cfg);
  for (let g = 0; g < slots.clades.length; g++) {
    const idx = slots.clades[g];
    if (idx < 0 || !data[idx]) continue;

    const group = groups[g] || { x: [], y: [] };
    const o = orientXY(group.x, group.y, cfg.orientation);
    // Fresh objects and fresh arrays: Plotly.react short-circuits its diff on
    // reference identity, so mutating in place would be a silent no-op.
    data[idx] = Object.assign({}, data[idx], { x: o.x, y: o.y });
  }

  if (slots.segments >= 0 && data[slots.segments]) {
    const segs = buildSegments(cfg.nodes, cfg.leaves, cut.expanded);
    const o = orientXY(segs.x, segs.y, cfg.orientation);
    data[slots.segments] = Object.assign({}, data[slots.segments], {
      x: o.x,
      y: o.y
    });
  }
  return data;
}

/**
 * Build the layout for a cut. Tick changes ride along here rather than in a
 * separate Plotly.relayout call -- see the loop-safety note at the top.
 */
function buildLayout(el, cfg, window_) {
  const layout = Object.assign({}, el.layout);
  const horizontal = cfg.orientation === 'left' || cfg.orientation === 'right';
  const axisName = horizontal ? 'yaxis' : 'xaxis';

  // Holding uirevision constant is what preserves the user's zoom across the
  // data swap; without it react resets the view on every update.
  layout.uirevision = cfg.uirevision || 'plotly-dendro';

  const ticks = buildTicks(cfg.leaves, window_, cfg);
  layout[axisName] = Object.assign({}, layout[axisName], {
    tickmode: 'array',
    tickvals: ticks.tickvals,
    ticktext: ticks.ticktext
  });

  return layout;
}

/**
 * Remove listeners and state.
 */
function destroy(el) {
  const state = el._dendro;
  if (!state) return;

  if (state.timer) {
    clearTimeout(state.timer);
  }
  if (state.handler && typeof el.removeListener === 'function') {
    el.removeListener('plotly_relayout', state.handler);
  }
  delete el._dendro;
}

window.plotlyDendro = {
  init: init,
  destroy: destroy,
  // Exposed for testing
  _apply: apply,
  _leafAxisRange: leafAxisRange,
  _DEBOUNCE_MS: DEBOUNCE_MS
};
