#!/usr/bin/env python3
"""
Generate Python plotly dendrogram reference fixtures for plotly.dendro Phase 4 tests.

Run once from the project root using the plotly_dendro conda environment:
    conda run -n plotly_dendro python tests/testthat/fixtures/generate_fixtures.py

To create the environment from scratch:
    conda env create -f tests/testthat/fixtures/environment.yml

Pinned versions (see environment.yml):
    plotly==6.6.0  scipy==1.17.1  numpy==2.4.2

Outputs three JSON files in this directory:
    tiny4_thresh_NULL.json   -- color_threshold = None  (all one color)
    tiny4_thresh_2.json      -- color_threshold = 2     (mid-split)
    tiny4_thresh_999.json    -- color_threshold = 999   (above max, all one color)

Each file is a JSON array of trace dicts, one per branch segment:
    [{"x": [x0,x1,x2,x3], "y": [y0,y1,y2,y3], "color": "rgb(r,g,b)"}, ...]

The tiny4 matrix matches the R test helper-data.R definition:
    A = (1, 2)
    B = (3, 4)
    C = (1, 4)
    D = (2, 3)
with complete linkage and Euclidean distance — identical to hclust_complete(dist(tiny4)).
"""

import json
import os
import numpy as np
import scipy.cluster.hierarchy as sch
import scipy.spatial.distance as scs

# ── tiny4: matches R matrix(c(1,2,3,4,1,4,2,3), nrow=4, byrow=TRUE) ──────────
X = np.array([
    [1, 2],
    [3, 4],
    [1, 4],
    [2, 3],
], dtype=float)

LABELS = ["A", "B", "C", "D"]

# Default colorscale — matches Python plotly's _dendrogram.py default.
DEFAULT_COLORS = [
    "rgb(0,116,217)",    # b → blue
    "rgb(35,205,205)",   # c → cyan
    "rgb(61,153,112)",   # g → green
    "rgb(40,35,35)",     # k → black
    "rgb(133,20,75)",    # m → magenta
    "rgb(255,65,54)",    # r → red
    "rgb(255,255,255)",  # w → white
    "rgb(255,220,0)",    # y → yellow
]

# New-style cyclic color names (scipy >= 1.5.0) → old-style letter
NEW_OLD = [
    ("C0", "b"), ("C1", "g"), ("C2", "r"), ("C3", "c"),
    ("C4", "m"), ("C5", "y"), ("C6", "k"), ("C7", "g"),
    ("C8", "r"), ("C9", "c"),
]

OLD_LETTER_TO_IDX = {"b": 0, "c": 1, "g": 2, "k": 3,
                      "m": 4, "r": 5, "w": 6, "y": 7}


def build_color_dict(colorscale=None):
    cs = colorscale if colorscale is not None else DEFAULT_COLORS
    letter_colors = {}
    old_letters = list("bcgkmrwy")
    for i, letter in enumerate(old_letters):
        letter_colors[letter] = cs[i] if i < len(cs) else DEFAULT_COLORS[i]
    for nc, oc in NEW_OLD:
        letter_colors[nc] = letter_colors.get(oc, DEFAULT_COLORS[0])
    return letter_colors


def make_traces(X, labels, color_threshold, linkage_method="complete"):
    d = scs.pdist(X)
    Z = sch.linkage(d, method=linkage_method)
    P = sch.dendrogram(
        Z,
        orientation="bottom",
        labels=labels,
        no_plot=True,
        color_threshold=color_threshold,
    )

    icoord     = np.array(P["icoord"])   # shape (n-1, 4)
    dcoord     = np.array(P["dcoord"])   # shape (n-1, 4)
    color_list = P["color_list"]         # list of n-1 color codes

    color_dict = build_color_dict()
    traces = []
    for i in range(len(icoord)):
        traces.append({
            "x":     icoord[i].tolist(),
            "y":     dcoord[i].tolist(),
            "color": color_dict.get(color_list[i], DEFAULT_COLORS[0]),
        })
    return traces


output_dir = os.path.dirname(os.path.abspath(__file__))

configs = [
    ("NULL", None),
    ("2",    2.0),
    ("999",  999.0),
]

for tag, thresh in configs:
    traces = make_traces(X, LABELS, thresh)
    fname  = os.path.join(output_dir, f"tiny4_thresh_{tag}.json")
    with open(fname, "w") as f:
        json.dump(traces, f, indent=2)
    print(f"Written: {fname}  ({len(traces)} traces, "
          f"{len(set(t['color'] for t in traces))} distinct colors)")

print("Done.")
