"""Cluster diseases and export reproducible comorbidity-network data and figures."""

import warnings
from pathlib import Path
import networkx as nx
import numpy as np
import pandas as pd

from matplotlib import pyplot as plt
from matplotlib.colors import to_hex
from scipy.cluster.hierarchy import dendrogram
from scipy.sparse.csgraph import laplacian
from sklearn.cluster import AgglomerativeClustering, SpectralClustering
from sklearn.metrics import silhouette_score

from config import label_name, src_dir

warnings.filterwarnings('ignore')

# ============================================================
# Output directory
# ============================================================

RESULT_DIR = Path("Result") / "cluster_result"
RESULT_DIR.mkdir(parents=True, exist_ok=True)

def cluster_result_process(names, data, y_cluster, suffix):
    """Save disease cluster assignments and within-cluster prevalence summaries."""
    names = np.array(names)

    # 每种疾病所属的 cluster
    result = np.array([names, y_cluster]).T
    np.savetxt(
        RESULT_DIR / f"disease_cluster_{suffix}_disease.csv",
        result,
        fmt='%s',
        delimiter=','
    )

    print(
        f"\t\t{suffix}算法共聚类得到"
        f"{len(np.unique(y_cluster))}个疾病组合！"
    )

    # 每个 cluster 的疾病组成及患者数
    result = [['疾病组合', '疾病数量', '至少患一种病', '同时患所有病']]

    for cluster in np.unique(y_cluster):
        combination_name = '+'.join(
            [str(v) for v in names[y_cluster == cluster]]
        )

        n_disease = np.sum(y_cluster == cluster)
        n_any = np.sum(
            np.any(
                data[y_cluster == cluster],
                axis=0
            )
        )
        n_all = np.sum(
            np.all(
                data[y_cluster == cluster],
                axis=0
            )
        )

        result.append([
            combination_name,
            n_disease,
            n_any,
            n_all
        ])

    np.savetxt(
        RESULT_DIR / f"disease_cluster_{suffix}_combination.csv",
        result,
        fmt='%s',
        delimiter=','
    )

def plot_dendrogram(model, names, threshold, suffix):
    """Plot a dendrogram from a fitted agglomerative clustering model."""
    # 层次聚类模型的训练结果本质上是一棵树
    # 假设有n_samples个样本，则会有n_samples-1个非叶节点
    # 显式构建 counts 数组统计每个树节点的样本数
    # model.children_中的元素由两部分构成，[0, n_samples-1]是样本，[n_samples, 2*n_samples-3]是非叶节点
    counts = np.zeros(model.children_.shape[0])
    n_samples = len(model.labels_)
    for i, merge in enumerate(model.children_): # 遍历每个非叶节点，得到一个具有两个元素的列表merge，分别表示其左右子节点
        current_count = 0
        for child_idx in merge:                 # 遍历每个非叶节点中存储的子结点索引
            if child_idx < n_samples:           # 若该索引小于n_samples，则说明是原始的样本（即叶结点）
                current_count += 1
            else:                               # 反之则为非叶节点，并且它在counts中的子结点序号是child_idx - n_samples
                current_count += counts[child_idx - n_samples]
        counts[i] = current_count

    # linkage_matrix记录了聚类过程中各个子簇的合并顺序、距离信息和样本数量
    # 其每一行有4个元素，分别表示第一个子簇、第二个子簇、这来那个歌子簇的合并距离、新簇的总样本数
    linkage_matrix = np.column_stack(
        [model.children_, model.distances_, counts]
    ).astype(float)

    # 根据聚类情况绘制树状图；横线高度越低，表示相应疾病越早被合并
    plt.figure(figsize=(10, 30))
    dendrogram(linkage_matrix, labels=names, leaf_rotation=90, color_threshold=threshold, above_threshold_color='black')
    plt.xlabel("Number of points in node (or index of point if no parenthesis).")
    plt.savefig(
        RESULT_DIR / f"disease_cluster_{suffix}_show.png",
        dpi=600,
        bbox_inches="tight"
    )
    plt.close()


def agg_clustering(data_df, corr_type):
    """Cluster diseases with agglomerative clustering and export its graphs."""
    algo = 'agg'
    agg_threshold = 350
    suffix = f'{algo}_distance_threshold={agg_threshold}'

    data = data_df.values.T
    names = list(data_df.columns)

    # 训练聚类模型
    # n_clusters参数表示要聚类成的簇数量
    # distance_threshold表示两个簇之间的最小距离，它会影响最终聚类出多少个组合
    print("训练聚类模型")
    model = AgglomerativeClustering(n_clusters=None, distance_threshold=agg_threshold)
    model.fit(data)
    y_cluster = model.labels_   # 得到聚类结果

    # 处理并保存聚类结果
    print("\t处理并保存聚类结果")
    cluster_result_process(names, data, y_cluster, suffix)

    # 可视化聚类结果
    print("\t可视化聚类结果")
    plot_dendrogram(model, names, agg_threshold, suffix)

    # 可视化疾病关联网络（无向图）
    print("\t可视化疾病关联网络（无向图）")
    draw_undirected_disease_graph(data_df, corr_type, y_cluster, suffix, show_threshold=0.0)

    # 可视化疾病关联网络（有向图）
    print("\t可视化疾病关联网络（有向图）")
    draw_directed_disease_graph(data_df, y_cluster, suffix, show_threshold=0.1)

    return y_cluster


def spe_clustering(data_df, corr_type):
    """Cluster diseases with spectral clustering based on pairwise association."""
    algo = 'spe'
    spe_n_clusters = 6
    suffix = f'{algo}_n_cluster={spe_n_clusters}'

    data = data_df.values.T
    names = list(data_df.columns)

    # 训练聚类模型
    # n_clusters参数表示要聚类成的簇数量
    print("\t训练聚类模型")
    model = SpectralClustering(n_clusters=spe_n_clusters, random_state=42)
    model.fit(data)
    y_cluster = model.labels_  # 得到聚类结果

    # 处理并保存聚类结果
    print("\t处理并保存聚类结果")
    cluster_result_process(names, data, y_cluster, suffix)

    # 可视化疾病关联网络（无向图）
    print("\t可视化疾病关联网络（无向图）")
    draw_undirected_disease_graph(data_df, corr_type, y_cluster, suffix, show_threshold=0.0)

    # 可视化疾病关联网络（有向图）
    print("\t可视化疾病关联网络（有向图）")
    draw_directed_disease_graph(data_df, y_cluster, suffix, show_threshold=0.1)

    return y_cluster


def get_disease_color_map(names, sort_indices):
    """Assign stable colors to diseases in the supplied display order."""
    # 为每个疾病使用一个颜色进行表示
    tab20 = plt.cm.tab20.colors  # 前20种
    tab20b = plt.cm.tab20b.colors  # 中20种
    tab20c = plt.cm.tab20c.colors  # 后20种
    all_colors = np.vstack([tab20, tab20b, tab20c])  # 合并为60种颜色
    colors = all_colors[:len(names)]  # 取前36种
    # 建立"疾病-颜色"映射
    node_to_color = {name: colors[sort_indices[i]] for i, name in enumerate(names)}
    return node_to_color


def save_disease_graph_data(
        graph_type, suffix, names, corr_mat, graph, pos, freq,
        node_to_cluster, node_to_color):
    """Export graph nodes, edges, and the complete association matrix."""
    output_prefix = RESULT_DIR / f"{graph_type}_disease_graph_{suffix}"

    # 保留完整矩阵，使 R 端可以换一个阈值后重新筛选边。
    corr_df = pd.DataFrame(corr_mat, index=names, columns=names)
    corr_df.index.name = 'disease'
    corr_df.to_csv(f"{output_prefix}_corr_mat.csv")

    # 节点表保留大小、颜色和坐标所需的全部信息。
    node_df = pd.DataFrame([
        {
            'disease': name,
            'node_id': node_i,
            'cluster': node_to_cluster[name],
            'frequency': freq[name],
            'color': to_hex(node_to_color[name]),
            'x': pos[name][0],
            'y': pos[name][1],
        }
        for node_i, name in enumerate(names)
    ])
    node_df.to_csv(f"{output_prefix}_nodes.csv", index=False)

    # 边表只包含通过 show_threshold 筛选、实际进入当前图的边。
    edge_df = pd.DataFrame([
        {
            'source': source,
            'target': target,
            'effect': attributes['effect'],
            'layout_weight': attributes['weight'],
            'edge_width': 5 * attributes['effect'],
        }
        for source, target, attributes in graph.edges(data=True)
    ], columns=['source', 'target', 'effect', 'layout_weight', 'edge_width'])
    edge_df.to_csv(f"{output_prefix}_edges.csv", index=False)

    print(
        f"\t\t已保存 R 重绘数据：{output_prefix}_corr_mat.csv、"
        f"{output_prefix}_nodes.csv、{output_prefix}_edges.csv"
    )


def draw_undirected_disease_graph(
        data_df, corr_type, y_cluster, suffix, show_threshold=0.0,
        corr_mat=None):
    """Draw and export the undirected disease-association network."""

    names = np.array(list(data_df.columns))

    # Alternative clustering views use absolute Pearson correlation. The
    # adopted eight-pattern solution passes its symmetric affinity explicitly.
    if corr_mat is None:
        corr_mat = abs(data_df.corr(method=corr_type).values)
    else:
        corr_mat = np.asarray(corr_mat, dtype=float)
    # 对 0/1 疾病变量求均值，得到每种疾病在样本中的发生频率（患病比例）。
    freq = dict(pd.Series(data_df.mean()))
    # 频率排序只决定图例顺序和颜色分配，不改变关联矩阵中的疾病顺序。
    sort_indices = np.argsort([freq[name] for name in names])[::-1]

    # 建立"疾病-颜色"映射
    node_to_color = get_disease_color_map(names, sort_indices)

    # 建立"疾病-类簇"映射
    node_to_cluster = {name: y_cluster[i] for i, name in enumerate(names)}

    # 构造无向网络：每种疾病是一个节点，每个达到阈值的疾病对是一条边。
    G = nx.Graph()
    G.add_nodes_from(names)
    for i in range(len(names)):
        for j in range(i+1, len(names)):
            # 小于阈值的边不进入图；等于阈值的边仍然保留。
            if abs(corr_mat[i, j]) < show_threshold:
                continue
            # weight 只控制节点布局：同一聚类的疾病会被更强地拉到一起。
            # effect 才是疾病关联强度，并用于决定线条宽度。
            if y_cluster[i] == y_cluster[j]:
                G.add_edge(names[i], names[j], effect=corr_mat[i, j], weight=10.0)
            else:
                G.add_edge(names[i], names[j], effect=corr_mat[i, j], weight=0.1)

    # spring_layout 类似把节点和边看作弹簧；seed 固定后，每次运行布局可重复。
    pos = nx.spring_layout(
        G,
        k=0.3,  # 节点间最优距离
        weight='weight',  # 使用权重
        iterations=100,  # 更多迭代使布局稳定
        seed=42
    )

    # 保存实际参与绘图的中间数据，便于在 R 中复现或重新设计图形
    save_disease_graph_data(
        'undirected', suffix, names, corr_mat, G, pos, freq,
        node_to_cluster, node_to_color
    )

    plt.figure(figsize=(10, 8))
    node_colors = [node_to_color[node] for node in G.nodes()]
    # node_labels = [node_to_cluster[node] for node in G.nodes()]
    # 节点内只显示简短数字；数字与疾病名称的对应关系见图例和 nodes.csv。
    node_labels = {node: str(n_i) for n_i, node in enumerate(G.nodes())}
    node_sizes = [5000 * freq[node] for node in G.nodes()]  # 根据疾病频率来设置该节点的大小
    edge_sizes = [5 * G.edges[edge]['effect'] for edge in G.edges()]    # 根据疾病关联来设置节点连线粗细
    # 绘制网络
    nx.draw(
        G,
        pos,
        node_color=node_colors,
        with_labels=True,       # 控制是否显示节点标签
        labels=node_labels,
        node_size=node_sizes,
        edge_color='gray',
        width=edge_sizes,
    )
    # 绘制图例
    legend_handles = [
        plt.Line2D(
            [0], [0],
            marker='o',
            color='w',
            markerfacecolor=node_colors[i],
            markersize=8,
            label=names[i]
        ) for i in sort_indices
    ]
    plt.legend(
        handles=legend_handles,
        title="Node Colors",
        bbox_to_anchor=(1.05, 1),
        loc='upper left',
        ncol=2  # 分两列显示
    )
    # 保存图像
    plt.savefig(
        RESULT_DIR / f"undirected_disease_graph_{suffix}.png",
        dpi=600,
        bbox_inches="tight"
    )
    plt.close()


def calculate_corr_mat(data, time, names):
    """Calculate and save conditional and symmetric disease affinities."""
    n_disease = len(names)

    # conditional_mat[i, j] = P(disease j | disease i).
    conditional_mat = np.zeros((n_disease, n_disease))
    for i in range(n_disease):
        mask_i = data[i] == 1
        total_i = np.sum(mask_i)  # 患得疾病A的总患者数

        if total_i == 0:  # 正常情况下，A疾病应该不会出现没有患者患病的情况（除非数据收集时出现问题）
            raise ValueError(f"Exist column {names[i]} all 0!")
        else:
            for j in range(n_disease):
                if i == j:
                    continue
                # 当前版本：只要求同一参与者同时患疾病 i 和疾病 j，不考虑时间顺序。
                mask_i_j = mask_i & (data[j] == 1)

                # 如需限定“疾病 j 晚于疾病 i”，在确认研究定义后改用下一行：
                # mask_i_j = mask_i & (data[j] == 1) & (time[j] > time[i])
                conditional_mat[i, j] = np.sum(mask_i_j) / total_i

    conditional_df = pd.DataFrame(
        conditional_mat,
        columns=names,
        index=names,
    )
    conditional_df.to_csv(
        RESULT_DIR / "directed_disease_corr_mat.csv"
    )

    # Adopted affinity: W_ij = [P(j | i) + P(i | j)] / 2.
    affinity_mat = (conditional_mat + conditional_mat.T) / 2.0
    np.fill_diagonal(affinity_mat, 0.0)
    affinity_df = pd.DataFrame(
        affinity_mat,
        columns=names,
        index=names,
    )
    affinity_df.to_csv(
        RESULT_DIR / "disease_affinity_mat.csv"
    )
    return affinity_mat


def draw_directed_disease_graph(data_df, y_cluster, suffix, show_threshold=0.0):
    """Draw and export the directed conditional-comorbidity network."""

    names = np.array(list(data_df.columns))
    n_disease = len(names)

    # 对 0/1 疾病变量求均值，得到患病比例，用来设置节点大小。
    freq = dict(pd.Series(data_df.mean()))
    # 根据发生频率对疾病进行排序
    sort_indices = np.argsort([freq[name] for name in names])[::-1]

    # 原始有向矩阵由 calculate_corr_mat 生成。
    corr_df = pd.read_csv(
        RESULT_DIR / "directed_disease_corr_mat.csv",
        index_col=0
    )
    # 按 data_df 的疾病顺序显式对齐行列，防止 CSV 顺序变化后边的含义错位
    corr_mat = corr_df.loc[names, names].values

    # 建立"疾病-颜色"映射
    node_to_color = get_disease_color_map(names, sort_indices)

    # 建立"疾病-类簇"映射
    node_to_cluster = {name: y_cluster[i] for i, name in enumerate(names)}

    # 构造有向网络：矩阵的行疾病为箭头起点，列疾病为箭头终点。
    G = nx.DiGraph()
    G.add_nodes_from(names)
    for i in range(len(names)):
        for j in range(n_disease):
            if i == j:
                continue

            # 小于阈值的边不进入图；等于阈值的边仍然保留。
            if abs(corr_mat[i, j]) < show_threshold:
                continue
            # weight 只影响布局，effect 是条件共病比例并决定线宽。
            if y_cluster[i] == y_cluster[j]:
                G.add_edge(names[i], names[j], effect=corr_mat[i, j], weight=10.0)
            else:
                G.add_edge(names[i], names[j], effect=corr_mat[i, j], weight=0.1)

    # 固定 seed，确保相同数据和参数可得到相同的布局。
    pos = nx.spring_layout(
        G,
        k=0.3,  # 节点间最优距离
        weight='weight',  # 使用权重
        iterations=100,  # 更多迭代使布局稳定
        seed=42
    )

    # 保存实际参与绘图的中间数据，便于在 R 中复现或重新设计图形
    save_disease_graph_data(
        'directed', suffix, names, corr_mat, G, pos, freq,
        node_to_cluster, node_to_color
    )

    plt.figure(figsize=(10, 8))
    node_colors = [node_to_color[node] for node in G.nodes()]
    # node_labels = [node_to_cluster[node] for node in G.nodes()]
    # 节点内显示从 0 开始的编号；疾病名称见图例和 nodes.csv。
    node_labels = {node: str(n_i) for n_i, node in enumerate(G.nodes())}
    node_sizes = [5000 * freq[node] for node in G.nodes()]  # 根据疾病频率来设置该节点的大小
    edge_sizes = [5 * G.edges[edge]['effect'] for edge in G.edges()]    # 根据疾病关联来设置节点连线粗细
    # 绘制网络
    nx.draw(
        G,
        pos,
        node_color=node_colors,
        labels=node_labels,
        with_labels=True,       # 控制是否显示节点标签
        node_size=node_sizes,
        width=edge_sizes,
        edge_color='gray',
        arrows=True,
        arrowstyle="->",
        # 以下是新增的曲线参数
        connectionstyle="arc3,rad=0.1",  # 控制曲线弧度，（rad值建议取0.05-0.3），值越大越弯曲）
        # style="dashed"  # 可选：虚线样式
    )
    # 绘制图例
    legend_handles = [
        plt.Line2D(
            [0], [0],
            marker='o',
            color='w',
            markerfacecolor=node_colors[i],
            markersize=8,
            label=names[i]
        ) for i in sort_indices
    ]
    plt.legend(
        handles=legend_handles,
        title="Node Colors",
        bbox_to_anchor=(1.05, 1),
        loc='upper left',
        ncol=2  # 分两列显示
    )
    # 保存图像
    plt.savefig(
        RESULT_DIR / f"directed_disease_graph_{suffix}.png",
        dpi=600,
        bbox_inches="tight"
    )
    plt.close()



def evaluate_clustering_quality(corr_mat, y_cluster, n_clusters):
    """
    Quantitatively evaluate one spectral-clustering solution.

    Metrics
    -------
    silhouette:
        Calculated from distance = 1 - affinity.
        Higher is better.

    within_affinity:
        Mean affinity for disease pairs assigned to the same cluster.
        Higher is better.

    between_affinity:
        Mean affinity for disease pairs assigned to different clusters.
        Lower is better.

    affinity_separation:
        within_affinity - between_affinity.
        Higher is better.

    modularity:
        Weighted Newman-Girvan modularity based on the full affinity graph.
        Higher is better.

    normalized_cut:
        Sum of cluster cut / cluster volume across all clusters.
        Lower is better.

    eigengap:
        lambda_(K+1) - lambda_K from the normalized graph Laplacian.
        Larger values provide greater spectral support for K.

    size_cv:
        Coefficient of variation of cluster sizes.
        Lower indicates more balanced cluster sizes.

    size_entropy:
        Normalized entropy of cluster-size distribution.
        Higher (closer to 1) indicates more balanced cluster sizes.
    """
    corr_mat = np.asarray(corr_mat, dtype=float)
    labels = np.asarray(y_cluster)

    if corr_mat.shape[0] != corr_mat.shape[1]:
        raise ValueError("Affinity matrix must be square.")

    if corr_mat.shape[0] != len(labels):
        raise ValueError("Affinity matrix and cluster labels have different lengths.")

    if len(np.unique(labels)) != n_clusters:
        raise ValueError(
            f"Requested K={n_clusters}, but clustering produced "
            f"{len(np.unique(labels))} unique clusters."
        )

    # Keep affinity within the valid range expected here.
    affinity = corr_mat.copy()
    affinity[~np.isfinite(affinity)] = 0.0
    affinity = np.clip(affinity, 0.0, 1.0)
    affinity = (affinity + affinity.T) / 2.0
    np.fill_diagonal(affinity, 0.0)

    # --------------------------------------------------------
    # 1. Silhouette based on precomputed distance
    # --------------------------------------------------------
    distance = 1.0 - affinity
    np.fill_diagonal(distance, 0.0)

    sil = silhouette_score(
        distance,
        labels,
        metric="precomputed"
    )

    # --------------------------------------------------------
    # 2. Mean within-cluster and between-cluster affinity
    # --------------------------------------------------------
    upper_i, upper_j = np.triu_indices(
        affinity.shape[0],
        k=1
    )

    pair_affinity = affinity[upper_i, upper_j]
    same_cluster = labels[upper_i] == labels[upper_j]

    within_affinity = (
        float(np.mean(pair_affinity[same_cluster]))
        if np.any(same_cluster)
        else np.nan
    )

    between_affinity = (
        float(np.mean(pair_affinity[~same_cluster]))
        if np.any(~same_cluster)
        else np.nan
    )

    affinity_separation = within_affinity - between_affinity

    # --------------------------------------------------------
    # 3. Weighted modularity
    # --------------------------------------------------------
    G = nx.Graph()
    G.add_nodes_from(range(affinity.shape[0]))

    for i in range(affinity.shape[0]):
        for j in range(i + 1, affinity.shape[0]):
            weight = float(affinity[i, j])
            if weight > 0:
                G.add_edge(i, j, weight=weight)

    communities = [
        set(np.where(labels == cluster)[0])
        for cluster in np.unique(labels)
    ]

    modularity = nx.algorithms.community.quality.modularity(
        G,
        communities,
        weight="weight"
    )

    # --------------------------------------------------------
    # 4. Normalized cut
    # --------------------------------------------------------
    degree = affinity.sum(axis=1)
    normalized_cut = 0.0

    for cluster in np.unique(labels):
        in_cluster = labels == cluster
        out_cluster = ~in_cluster

        cut_weight = affinity[
            np.ix_(in_cluster, out_cluster)
        ].sum()

        volume = degree[in_cluster].sum()

        if volume > 0:
            normalized_cut += cut_weight / volume

    # --------------------------------------------------------
    # 5. Eigengap from normalized graph Laplacian
    # --------------------------------------------------------
    L_norm = laplacian(
        affinity,
        normed=True
    )

    eigenvalues = np.linalg.eigvalsh(
        np.asarray(L_norm)
    )
    eigenvalues = np.sort(
        np.real(eigenvalues)
    )

    # Python indexing:
    # eigenvalues[K-1] = lambda_K
    # eigenvalues[K]   = lambda_(K+1)
    eigengap = (
        float(eigenvalues[n_clusters] - eigenvalues[n_clusters - 1])
        if n_clusters < len(eigenvalues)
        else np.nan
    )

    # --------------------------------------------------------
    # 6. Cluster-size balance
    # --------------------------------------------------------
    cluster_sizes = np.array(
        [
            np.sum(labels == cluster)
            for cluster in np.unique(labels)
        ],
        dtype=float
    )

    size_cv = float(
        np.std(cluster_sizes, ddof=0) /
        np.mean(cluster_sizes)
    )

    proportions = cluster_sizes / cluster_sizes.sum()

    size_entropy = float(
        -np.sum(
            proportions *
            np.log(proportions)
        ) /
        np.log(n_clusters)
    )

    result = {
        "K": int(n_clusters),
        "silhouette": float(sil),
        "within_affinity": within_affinity,
        "between_affinity": between_affinity,
        "affinity_separation": affinity_separation,
        "modularity": float(modularity),
        "normalized_cut": float(normalized_cut),
        "eigengap": eigengap,
        "min_cluster_size": int(cluster_sizes.min()),
        "max_cluster_size": int(cluster_sizes.max()),
        "size_cv": size_cv,
        "size_entropy": size_entropy,
    }

    return result


def save_clustering_quality_summary(quality_rows):
    """Save K=6-8 clustering-quality metrics and simple per-metric ranks."""
    quality_df = pd.DataFrame(quality_rows).sort_values("K").reset_index(drop=True)

    # Direction of preference:
    # higher is better for these metrics
    higher_better = [
        "silhouette",
        "within_affinity",
        "affinity_separation",
        "modularity",
        "eigengap",
        "size_entropy",
    ]

    # lower is better for these metrics
    lower_better = [
        "between_affinity",
        "normalized_cut",
        "size_cv",
    ]

    rank_cols = []

    for metric in higher_better:
        rank_col = f"rank_{metric}"
        quality_df[rank_col] = quality_df[metric].rank(
            ascending=False,
            method="min"
        )
        rank_cols.append(rank_col)

    for metric in lower_better:
        rank_col = f"rank_{metric}"
        quality_df[rank_col] = quality_df[metric].rank(
            ascending=True,
            method="min"
        )
        rank_cols.append(rank_col)

    # Descriptive composite rank only.
    # Do not treat this as a formal statistical criterion.
    quality_df["mean_rank"] = quality_df[rank_cols].mean(axis=1)

    quality_df.to_csv(
        RESULT_DIR / "clustering_quality_K6_K8.csv",
        index=False
    )

    print(
        "\n============================================================"
    )
    print("K=6-8 quantitative clustering-quality comparison")
    print("Higher is better: silhouette, within affinity, separation,")
    print("                  modularity, eigengap, size entropy")
    print("Lower is better:  between affinity, normalized cut, size CV")
    print("============================================================")
    print(
        quality_df[
            [
                "K",
                "silhouette",
                "within_affinity",
                "between_affinity",
                "affinity_separation",
                "modularity",
                "normalized_cut",
                "eigengap",
                "min_cluster_size",
                "max_cluster_size",
                "size_cv",
                "size_entropy",
                "mean_rank",
            ]
        ].to_string(index=False)
    )

    best_mean_rank = quality_df["mean_rank"].min()

    best_k = quality_df.loc[
        quality_df["mean_rank"] == best_mean_rank,
        "K"
    ].tolist()

    print(
        "\nBest K by descriptive mean rank:",
        ", ".join(map(str, best_k))
    )
    print(
        "NOTE: Final K should still combine quantitative performance "
        "with clinical interpretability."
    )

    return quality_df


def spe_clustering_with_corr_mat(data_df, corr_type, n_clusters):
    """Run spectral clustering from the symmetric conditional affinity."""
    algo = 'spe_corr'
    suffix = f'{algo}_n_cluster={n_clusters}'

    data = data_df.values.T
    names = list(data_df.columns)

    corr_df = pd.read_csv(
        RESULT_DIR / "disease_affinity_mat.csv",
        index_col=0
    )
    corr_mat = corr_df.loc[names, names].values

    # 训练聚类模型
    # n_clusters参数表示要聚类成的簇数量
    # distance_threshold表示两个簇之间的最小距离，它会影响最终聚类出多少个组合
    print("\t训练聚类模型")
    model = SpectralClustering(n_clusters=n_clusters, random_state=42, affinity='precomputed')
    y_cluster = model.fit_predict(corr_mat) # 得到聚类结果

    # 处理并保存聚类结果
    print("\t处理并保存聚类结果")
    cluster_result_process(names, data, y_cluster, suffix)

    # 可视化疾病关联网络（无向图）
    print("\t可视化疾病关联网络（无向图）")
    draw_undirected_disease_graph(
        data_df,
        corr_type,
        y_cluster,
        suffix,
        show_threshold=0.0,
        corr_mat=corr_mat,
    )

    return y_cluster


if __name__ == '__main__':
    # 要读取的疾病列名
    column_names = np.array(['Anaemia', 'Angina', 'Anxiety_or_panic_attacks', 'Atrial_fibrillation', 'Asthma',
                             'Bronchiectasis', 'Chronic_kidney_disease', 'Chronic_obstructive_pulmonary_disease', 'Chronic_sinusitis',
                             'Cirrhosis', 'Dementia', 'Depression', 'Diabetes', 'Eczema_or_dermatitis',
                             'Epilepsy', 'Glaucoma', 'Heart_failure', 'Hepatitis', 'Hypertension',
                             'Inflammatory_bowel_disease', 'Irritable_bowel_syndrome', 'Meningitis', 'Migraine', 'Multiple_sclerosis',
                             'Myocardial_infarction', 'Osteoporosis', 'Parkinson_disease', 'Peripheral_vascular_disease', 'Prostate_problem',
                             'Rheumatoid_arthritis', 'Schizophrenia', 'Stroke', 'Thyroid_problem', 'Tuberculosis', 'Vestibular_disorder', 'overall_cancer'])
    time_column_names = np.array([name+'_time' if name != 'overall_cancer' else 'cancer_date' for name in column_names])
    label_data = pd.read_csv(f"{src_dir}/{label_name}", index_col=0, encoding="gbk")
    data_df = label_data[column_names]
    time_df = label_data[time_column_names]
    for col in time_column_names:  # time_column_names 需是列表（如 ["date1", "date2"]）
        time_df[col] = pd.to_datetime(
            time_df[col],
            format="%Y/%m/%d",
            errors='coerce'  # 关键参数：将无效日期转为NaT
        )

    # 评估疾病关联的方法
    corr_type = 'pearson'

    # 计算方向性条件概率及其对称化疾病亲和矩阵。
    print("计算疾病之间的条件概率和对称亲和矩阵")
    calculate_corr_mat(data_df.values.T, time_df.values.T, list(data_df.columns))

    ## ============================================================
    # Final clustering:
    # symmetric conditional co-occurrence affinity matrix
    # Evaluate only K = 6, 7, 8
    #  ============================================================

    # ============================================================
    # Final clustering:
    # symmetric conditional co-occurrence affinity matrix
    # Evaluate only K = 6, 7, 8
    # ============================================================

    # Read the same symmetric affinity matrix used for clustering.
    quality_corr_df = pd.read_csv(
        RESULT_DIR / "disease_affinity_mat.csv",
        index_col=0
    )
    quality_corr_mat = quality_corr_df.loc[
        data_df.columns,
        data_df.columns
    ].values

    quality_rows = []

    for n_clusters in range(6, 9):
        print(
            f"\n谱聚类："
            f"对称条件共现亲和矩阵，K={n_clusters}"
        )

        y_cluster = spe_clustering_with_corr_mat(
            data_df,
            corr_type,
            n_clusters
        )

        quality_result = evaluate_clustering_quality(
            quality_corr_mat,
            y_cluster,
            n_clusters
        )

        quality_rows.append(
            quality_result
        )

    save_clustering_quality_summary(
        quality_rows
    )
