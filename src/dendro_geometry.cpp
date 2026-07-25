#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

using namespace Rcpp;

namespace {

struct GeometryPlan {
  int m;
  int n;
  std::vector<double> leaf_x;
  std::vector<double> leaf_y;
  std::vector<double> node_x;
  std::vector<int> members;
  std::vector<double> midpoint;
};

inline bool finite_number(double value) {
  return R_FINITE(value);
}

void validate_common_inputs(
    const IntegerMatrix& merge,
    const NumericVector& height,
    const IntegerVector& order
) {
  if (merge.ncol() != 2) {
    stop("Invalid hclust: merge must be an integer matrix with two columns.");
  }

  const int m = merge.nrow();
  if (m < 1) {
    stop("Invalid hclust: at least two leaves are required.");
  }
  const int n = m + 1;

  if (height.size() != m) {
    stop("Invalid hclust: height must have one value per merge row.");
  }
  if (order.size() != n) {
    stop("Invalid hclust: order must have one entry per leaf.");
  }
  if (static_cast<R_xlen_t>(m) >
      std::numeric_limits<R_xlen_t>::max() / 4) {
    stop("Invalid hclust: requested segment output is too large.");
  }

  std::vector<unsigned char> seen_order(n, 0);
  for (int position = 0; position < n; ++position) {
    const int leaf = order[position];
    if (leaf == NA_INTEGER || leaf < 1 || leaf > n) {
      stop("Invalid hclust: order must be a permutation of 1..n.");
    }
    if (seen_order[leaf - 1]) {
      stop("Invalid hclust: order contains duplicate leaf references.");
    }
    seen_order[leaf - 1] = 1;
  }

  std::vector<int> leaf_parents(n, 0);
  std::vector<int> node_parents(m, 0);
  for (int row = 0; row < m; ++row) {
    if (!finite_number(height[row])) {
      stop("Invalid hclust: height values must be finite.");
    }
    for (int column = 0; column < 2; ++column) {
      const int child = merge(row, column);
      if (child == NA_INTEGER || child == 0) {
        stop("Invalid hclust: merge references must be non-zero integers.");
      }
      if (child < 0) {
        const int leaf = -child;
        if (leaf < 1 || leaf > n) {
          stop("Invalid hclust: leaf merge reference is out of range.");
        }
        ++leaf_parents[leaf - 1];
      } else {
        if (child > row) {
          stop("Invalid hclust: internal merge references must point to earlier rows.");
        }
        ++node_parents[child - 1];
      }
    }
  }

  for (int leaf = 0; leaf < n; ++leaf) {
    if (leaf_parents[leaf] != 1) {
      stop("Invalid hclust: every leaf must have exactly one parent.");
    }
  }
  for (int node = 0; node < m - 1; ++node) {
    if (node_parents[node] != 1) {
      stop("Invalid hclust: every non-root merge must have exactly one parent.");
    }
  }
  if (node_parents[m - 1] != 0) {
    stop("Invalid hclust: the final merge row must be the root.");
  }
}

inline int child_members(int child, const std::vector<int>& members) {
  return child < 0 ? 1 : members[child - 1];
}

inline double child_midpoint(
    int child,
    const std::vector<double>& midpoint
) {
  return child < 0 ? 0.0 : midpoint[child - 1];
}

inline int child_min_position(
    int child,
    const std::vector<int>& leaf_position,
    const std::vector<int>& min_position
) {
  return child < 0 ? leaf_position[-child - 1] : min_position[child - 1];
}

inline int child_max_position(
    int child,
    const std::vector<int>& leaf_position,
    const std::vector<int>& max_position
) {
  return child < 0 ? leaf_position[-child - 1] : max_position[child - 1];
}

inline double child_x(int child, const GeometryPlan& plan) {
  return child < 0 ? plan.leaf_x[-child - 1] : plan.node_x[child - 1];
}

inline double child_y(
    int child,
    const NumericVector& height,
    const GeometryPlan& plan
) {
  return child < 0 ? plan.leaf_y[-child - 1] : height[child - 1];
}

GeometryPlan make_plan(
    const IntegerMatrix& merge,
    const NumericVector& height,
    const IntegerVector& order,
    double leaf_drop
) {
  validate_common_inputs(merge, height, order);
  if (ISNAN(leaf_drop)) {
    stop("leaf_drop must not be NA or NaN.");
  }

  const int m = merge.nrow();
  const int n = m + 1;
  GeometryPlan plan;
  plan.m = m;
  plan.n = n;
  plan.leaf_x.resize(n);
  plan.leaf_y.assign(n, 0.0);
  plan.node_x.resize(m);
  plan.members.resize(m);
  plan.midpoint.resize(m);

  std::vector<int> leaf_position(n);
  std::vector<int> min_position(m);
  std::vector<int> max_position(m);
  for (int position = 0; position < n; ++position) {
    const int leaf_index = order[position] - 1;
    plan.leaf_x[leaf_index] = static_cast<double>(position + 1);
    leaf_position[leaf_index] = position + 1;
  }

  for (int row = 0; row < m; ++row) {
    const int left = merge(row, 0);
    const int right = merge(row, 1);
    const int left_members = child_members(left, plan.members);
    const int right_members = child_members(right, plan.members);
    const int left_min = child_min_position(
      left, leaf_position, min_position
    );
    const int left_max = child_max_position(
      left, leaf_position, max_position
    );
    const int right_min = child_min_position(
      right, leaf_position, min_position
    );
    const int right_max = child_max_position(
      right, leaf_position, max_position
    );

    if (left_max + 1 != right_min ||
        left_max - left_min + 1 != left_members ||
        right_max - right_min + 1 != right_members) {
      stop("Invalid hclust: order is inconsistent with merge subtree ordering.");
    }

    plan.members[row] = left_members + right_members;
    min_position[row] = left_min;
    max_position[row] = right_max;

    // This is base R's binary dendrogram midpoint recurrence. The right
    // child midpoint is offset by the full membership of the left child.
    plan.midpoint[row] = (
      child_midpoint(left, plan.midpoint) +
      (static_cast<double>(left_members) +
       child_midpoint(right, plan.midpoint))
    ) / 2.0;
    plan.node_x[row] =
      static_cast<double>(left_min) + plan.midpoint[row];

    for (int column = 0; column < 2; ++column) {
      const int child = merge(row, column);
      if (child < 0) {
        const int leaf_index = -child - 1;
        plan.leaf_y[leaf_index] = leaf_drop < 0.0
          ? 0.0
          : std::max(height[row] - leaf_drop, 0.0);
      }
    }
  }

  if (min_position[m - 1] != 1 ||
      max_position[m - 1] != n ||
      plan.members[m - 1] != n) {
    stop("Invalid hclust: final merge does not span all leaves.");
  }
  return plan;
}

struct Frame {
  int node;
  int state;
};

template <typename EmitEdge>
void walk_edges(
    const IntegerMatrix& merge,
    const GeometryPlan& plan,
    EmitEdge emit_edge
) {
  std::vector<Frame> stack;
  stack.reserve(plan.m);
  stack.push_back(Frame{plan.m, 0});

  while (!stack.empty()) {
    Frame& frame = stack.back();
    const int row = frame.node - 1;

    if (frame.state == 0) {
      frame.state = 1;
      const int child = merge(row, 0);
      emit_edge(frame.node, child);
      if (child > 0) {
        stack.push_back(Frame{child, 0});
      }
    } else if (frame.state == 1) {
      frame.state = 2;
      const int child = merge(row, 1);
      emit_edge(frame.node, child);
      if (child > 0) {
        stack.push_back(Frame{child, 0});
      }
    } else {
      stack.pop_back();
    }
  }
}

DataFrame make_segments(
    const IntegerMatrix& merge,
    const NumericVector& height,
    const GeometryPlan& plan
) {
  const R_xlen_t output_size = static_cast<R_xlen_t>(plan.m) * 4;
  NumericVector x(output_size);
  NumericVector y(output_size);
  NumericVector xend(output_size);
  NumericVector yend(output_size);
  R_xlen_t cursor = 0;

  walk_edges(merge, plan, [&](int parent, int child) {
    const double parent_x = plan.node_x[parent - 1];
    const double parent_y = height[parent - 1];
    const double edge_child_x = child_x(child, plan);
    const double edge_child_y = child_y(child, height, plan);

    x[cursor] = parent_x;
    y[cursor] = parent_y;
    xend[cursor] = edge_child_x;
    yend[cursor] = parent_y;
    ++cursor;

    x[cursor] = edge_child_x;
    y[cursor] = parent_y;
    xend[cursor] = edge_child_x;
    yend[cursor] = edge_child_y;
    ++cursor;
  });

  if (cursor != output_size) {
    stop("Internal error: segment output length mismatch.");
  }
  return DataFrame::create(
    _["x"] = x,
    _["y"] = y,
    _["xend"] = xend,
    _["yend"] = yend,
    _["stringsAsFactors"] = false
  );
}

DataFrame make_labels(
    const IntegerVector& order,
    const CharacterVector& labels,
    const GeometryPlan& plan
) {
  NumericVector x(plan.n);
  NumericVector y(plan.n);
  CharacterVector label(plan.n);
  for (int position = 0; position < plan.n; ++position) {
    const int leaf_index = order[position] - 1;
    x[position] = static_cast<double>(position + 1);
    y[position] = plan.leaf_y[leaf_index];
    label[position] = labels[leaf_index];
  }
  return DataFrame::create(
    _["x"] = x,
    _["y"] = y,
    _["label"] = label,
    _["stringsAsFactors"] = false
  );
}

DataFrame make_nodes(
    const NumericVector& height,
    const GeometryPlan& plan,
    bool need_nodes
) {
  if (!need_nodes) {
    return DataFrame::create(
      _["x"] = NumericVector(0),
      _["y"] = NumericVector(0),
      _["members"] = IntegerVector(0),
      _["height"] = NumericVector(0),
      _["stringsAsFactors"] = false
    );
  }

  std::vector<int> index(plan.m);
  for (int row = 0; row < plan.m; ++row) {
    index[row] = row;
  }
  std::stable_sort(index.begin(), index.end(), [&](int left, int right) {
    if (height[left] < height[right]) {
      return true;
    }
    if (height[left] > height[right]) {
      return false;
    }
    return plan.node_x[left] < plan.node_x[right];
  });

  int unique_count = 0;
  for (int output = 0; output < plan.m; ++output) {
    const int row = index[output];
    if (output == 0 ||
        height[row] != height[index[output - 1]] ||
        plan.node_x[row] != plan.node_x[index[output - 1]]) {
      ++unique_count;
    }
  }

  NumericVector x(unique_count);
  NumericVector y(unique_count);
  IntegerVector members(unique_count, NA_INTEGER);
  NumericVector node_height(unique_count);
  int unique_output = 0;
  for (int output = 0; output < plan.m; ++output) {
    const int row = index[output];
    if (output > 0 &&
        height[row] == height[index[output - 1]] &&
        plan.node_x[row] == plan.node_x[index[output - 1]]) {
      continue;
    }
    x[unique_output] = plan.node_x[row];
    y[unique_output] = height[row];
    node_height[unique_output] = height[row];
    ++unique_output;
  }
  return DataFrame::create(
    _["x"] = x,
    _["y"] = y,
    _["members"] = members,
    _["height"] = node_height,
    _["stringsAsFactors"] = false
  );
}

inline bool same_number(double left, double right) {
  return left == right;
}

} // namespace

// [[Rcpp::export]]
List cpp_hclust_geometry(
    IntegerMatrix merge,
    NumericVector height,
    IntegerVector order,
    CharacterVector labels,
    double leaf_drop,
    bool need_nodes
) {
  const GeometryPlan plan = make_plan(
    merge, height, order, leaf_drop
  );
  if (labels.size() != plan.n) {
    stop("labels must have one entry per leaf.");
  }

  return List::create(
    _["segments"] = make_segments(merge, height, plan),
    _["labels"] = make_labels(order, labels, plan),
    _["nodes"] = make_nodes(height, plan, need_nodes)
  );
}

// [[Rcpp::export]]
Nullable<IntegerVector> cpp_hclust_branch_ids(
    IntegerMatrix merge,
    NumericVector height,
    IntegerVector order,
    NumericVector segment_x,
    NumericVector segment_y,
    NumericVector segment_xend,
    NumericVector segment_yend,
    IntegerVector leaf_color_ids
) {
  const GeometryPlan plan = make_plan(
    merge, height, order, -1.0
  );
  const R_xlen_t output_size = static_cast<R_xlen_t>(plan.m) * 4;
  if (segment_x.size() != output_size ||
      segment_y.size() != output_size ||
      segment_xend.size() != output_size ||
      segment_yend.size() != output_size) {
    return R_NilValue;
  }
  if (leaf_color_ids.size() != plan.n) {
    stop("leaf_color_ids must have one entry per displayed leaf.");
  }

  std::vector<int> leaf_ids_by_original(plan.n);
  for (int position = 0; position < plan.n; ++position) {
    const int id = leaf_color_ids[position];
    if (id == NA_INTEGER || id < 1) {
      stop("leaf_color_ids must contain positive non-missing integers.");
    }
    leaf_ids_by_original[order[position] - 1] = id;
  }

  std::vector<int> uniform(plan.m, 0);
  for (int row = 0; row < plan.m; ++row) {
    const int left = merge(row, 0);
    const int right = merge(row, 1);
    const int left_id = left < 0
      ? leaf_ids_by_original[-left - 1]
      : uniform[left - 1];
    const int right_id = right < 0
      ? leaf_ids_by_original[-right - 1]
      : uniform[right - 1];
    uniform[row] = left_id > 0 && left_id == right_id ? left_id : 0;
  }

  IntegerVector output(output_size);
  R_xlen_t cursor = 0;
  bool canonical = true;
  walk_edges(merge, plan, [&](int parent, int child) {
    if (!canonical) {
      return;
    }
    const double parent_x = plan.node_x[parent - 1];
    const double parent_y = height[parent - 1];
    const double edge_child_x = child_x(child, plan);
    const double edge_child_y = child < 0
      ? segment_yend[cursor + 1]
      : height[child - 1];

    if (!finite_number(segment_x[cursor]) ||
        !finite_number(segment_y[cursor]) ||
        !finite_number(segment_xend[cursor]) ||
        !finite_number(segment_yend[cursor]) ||
        !finite_number(segment_x[cursor + 1]) ||
        !finite_number(segment_y[cursor + 1]) ||
        !finite_number(segment_xend[cursor + 1]) ||
        !finite_number(segment_yend[cursor + 1]) ||
        !same_number(segment_x[cursor], parent_x) ||
        !same_number(segment_y[cursor], parent_y) ||
        !same_number(segment_xend[cursor], edge_child_x) ||
        !same_number(segment_yend[cursor], parent_y) ||
        !same_number(segment_x[cursor + 1], edge_child_x) ||
        !same_number(segment_y[cursor + 1], parent_y) ||
        !same_number(segment_xend[cursor + 1], edge_child_x) ||
        !same_number(segment_yend[cursor + 1], edge_child_y)) {
      canonical = false;
      return;
    }

    const int child_id = child < 0
      ? leaf_ids_by_original[-child - 1]
      : uniform[child - 1];
    const int edge_id = child_id > 0 ? child_id : 1;
    output[cursor] = edge_id;
    output[cursor + 1] = edge_id;
    cursor += 2;
  });

  if (!canonical || cursor != output_size) {
    return R_NilValue;
  }
  return output;
}
