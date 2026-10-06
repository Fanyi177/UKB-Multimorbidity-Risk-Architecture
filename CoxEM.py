"""Implement the Cox ensemble model with adaptive event sampling."""


import pickle
import time
import joblib

import multiprocessing as mp
import numpy as np
import pandas as pd

from joblib import Parallel, delayed
from scipy.stats import pearsonr
from sksurv.linear_model import CoxnetSurvivalAnalysis, CoxPHSurvivalAnalysis

from config import ana_dir, tmp_dir, sele_type, fea_corr_type, max_ff_corr, max_fl_corr, n_fea_sel, fl_corr_type, min_fl_c_corr, make_dir
from config import mod_dir, n_sample, fold_type, local_sample, macro_r_type, macro_p_type, show_fitness, show_perf, log_dir, basic_model, macro_c_type
from config import (
    sample_ratio,
    rare_event_sample_ratio,
    rare_event_threshold,
    sample_with_replacement,
    special_retain_features,
    special_remove_features,
    coxnet_alpha_min_ratio_when_n_gt_p,
)

from utils import get_logger, generate_safe_eval_times, operation
from sksurv.metrics import cumulative_dynamic_auc, concordance_index_censored

class CoxEM:
    """Fit an ensemble of Cox base learners with bootstrap samples and adaptive feature subsets."""
    def __init__(self, c_type, target, task_id, n_estimators=10, random_state=None, n_jobs=None,
                 feature_map=None, use_valid=True, valid_metric='auc', verbose=0,
                 fixed_feature_set=False, compute_train_fitness=None, compute_eval_perf=None):
        """Initialize the Cox ensemble and its runtime options."""
        self.c_type = c_type
        self.target = target
        self.task_id = task_id
        self.n_estimators = n_estimators
        self.random_state = random_state
        self.n_jobs = n_jobs

        self.n_feature = None

        self.feature_names = None
        self.feature_map = feature_map

        self.verbose = verbose

        self.use_valid = use_valid
        self.valid_metric = valid_metric

        # Stage 3 can freeze the Stage 2-selected feature set so that CoxEM
        # does not perform another feature-selection step.
        self.fixed_feature_set = fixed_feature_set

        # Backward-compatible defaults for Stage 1/2.
        self.compute_train_fitness = (
            show_fitness if compute_train_fitness is None
            else bool(compute_train_fitness)
        )
        self.compute_eval_perf = (
            show_perf if compute_eval_perf is None
            else bool(compute_eval_perf)
        )

        self.sample_indices = None
        self.valid_indices = None
        self.feature_indices = None
        self.feature_indexes = []

        self.estimators = [None for _ in range(n_estimators)]
        self.weights = []

        self.tmp_path = make_dir(f"{tmp_dir}/{self.c_type}_{self.target}")
        self.ana_path = make_dir(f"{ana_dir}/{self.c_type}_{self.target}")

        self.logger = get_logger(f"{c_type}_{target}_CoxEM_{basic_model}_{task_id}", log_dir)

    def fit(self, x, y, x_valid=None, y_valid=None):
        """Fit all Cox base learners and derive normalized ensemble weights."""
        self.n_feature = x.shape[1]
        self.feature_names = list(x.columns)
        #raise ValueError(f"{self.feature_names}")
        #nklnlk

        # 固定随机数种子
        if self.random_state:
            np.random.seed(self.random_state)

        # 为每个基学习器生成训练样本索引
        self.sample_indices = self.generate_sample_indices(y)

        # 为每个基学习器生成验证样本索引
        if self.use_valid:
            self.valid_indices = self.generate_valid_indices(y)

        # 为每个基学习器生成特征索引
        # Stage 1/2 keep the original adaptive CoxEM selection.
        # Stage 3 uses fixed_feature_set=True so every base learner uses the
        # exact final Stage 2 feature set.
        if self.fixed_feature_set:
            self.feature_indices = [
                np.array(self.feature_names, dtype=object)
                for _ in range(self.n_estimators)
            ]
        else:
            self.feature_indices = self.generate_feature_indices(x, y)

        # 将micro名称转化为下标
        self.feature_indexes = [
            [x.columns.get_loc(col) for col in feature_indice]
            for feature_indice in self.feature_indices
        ]

        # 训练模型
        if self.n_jobs:
            # Multi-process
            if self.verbose >= 1:
                self.logger.info("Train trees in parallel...")
            results = Parallel(n_jobs=self.n_jobs)(
                delayed(self.fit_estimator)(
                    t_i,
                    x.loc[self.sample_indices[t_i], self.feature_indices[t_i]],
                    y[self.sample_indices[t_i]],
                    x.loc[self.valid_indices[t_i], self.feature_indices[t_i]] if self.use_valid else None,
                    y[self.valid_indices[t_i]] if self.use_valid else None,
                    x_valid.loc[:, self.feature_indices[t_i]] if x_valid is not None else None,
                    y_valid if y_valid is not None else None,
                )
                for t_i in range(self.n_estimators)
            )
        else:
            # Single-process
            if self.verbose >= 1:
                self.logger.info("Train trees in sequence...")
            results = [
                self.fit_estimator(
                    t_i,
                    x.loc[self.sample_indices[t_i], self.feature_indices[t_i]],
                    y[self.sample_indices[t_i]],
                    x.loc[self.valid_indices[t_i], self.feature_indices[t_i]] if self.use_valid else None,
                    y[self.valid_indices[t_i]] if self.use_valid else None,
                    x_valid.loc[:, self.feature_indices[t_i]] if x_valid is not None else None,
                    y_valid if y_valid is not None else None,
                )
                for t_i in range(self.n_estimators)
            ]

        # 提取训练好的基学习器及其权重
        weights = []
        for t_i in range(self.n_estimators):
            model_file = f"{mod_dir}/{self.target}_run_CoxEM_{basic_model}_{n_sample}_{fold_type}_{self.task_id}_{t_i}.pkl" if local_sample \
                else f"{mod_dir}/{self.target}_run_CoxEM_{basic_model}_{fold_type}_{self.task_id}_{t_i}.pkl"
            self.estimators[t_i] = joblib.load(model_file)

            fitness, weight, perf = results[t_i]
            weights.append(weight)

            output_str = f"Tree {t_i} trained"
            output_str += f", train {self.valid_metric}: {fitness:.4f}"
            output_str += f", valid {self.valid_metric}: {weight:.4f}"
            output_str += f", test {self.valid_metric}: {perf:.4f}"
            if self.verbose >= 2:
                self.logger.info(output_str)
        weights = np.asarray(weights, dtype=float)
        valid_weight_mask = np.isfinite(weights) & (weights > 0)
        if not np.any(valid_weight_mask):
            weights = np.ones(self.n_estimators, dtype=float)
        else:
            weights = np.where(valid_weight_mask, weights, 0.0)

        weight_sum = np.sum(weights)
        if (not np.isfinite(weight_sum)) or weight_sum <= 0:
            weights = np.ones(self.n_estimators, dtype=float)
            weight_sum = float(self.n_estimators)

        self.weights = weights / weight_sum

    def generate_sample_indices(self, y):
        """为每个 CoxEM 基学习器生成训练样本。

        抽样原则：
        1. 根据当前训练数据中的事件数自适应决定抽样比例：
           - events < rare_event_threshold：使用 rare_event_sample_ratio
           - events >= rare_event_threshold：使用 sample_ratio

        2. event 和 censor 保持原数据中的大致比例。

        3. 固定保留随访时间最长的若干 event/censor。

        4. 推荐同一个基学习器内部不重复抽样；
           不同基学习器之间允许抽到相同 participant。
        """

        # --------------------------------------------------------
        # 1. 分成事件组和删失组
        # --------------------------------------------------------
        true_group = [
            (i, val[1])
            for i, val in enumerate(y)
            if val[0] == True
        ]

        false_group = [
            (i, val[1])
            for i, val in enumerate(y)
            if val[0] == False
        ]

        if len(true_group) == 0:
            raise ValueError(
                f"CoxEM {self.c_type}/{self.target}: "
                "training data contain no events."
            )

        if len(false_group) == 0:
            raise ValueError(
                f"CoxEM {self.c_type}/{self.target}: "
                "training data contain no censored observations."
            )

        true_indices, true_values = map(
            np.array,
            zip(*true_group)
        )

        false_indices, false_values = map(
            np.array,
            zip(*false_group)
        )

        n_events = len(true_group)
        n_censored = len(false_group)

        # --------------------------------------------------------
        # 2. 根据事件数动态决定 sample ratio
        # --------------------------------------------------------
        if n_events < rare_event_threshold:
            current_sample_ratio = rare_event_sample_ratio
        else:
            current_sample_ratio = sample_ratio

        if self.verbose >= 1:
            self.logger.info(
                f"CoxEM sampling: events={n_events}, "
                f"censored={n_censored}, "
                f"sample_ratio={current_sample_ratio:.2f}, "
                f"replace_within_learner={sample_with_replacement}"
            )

        # --------------------------------------------------------
        # 3. 固定保留随访时间最长的样本
        # --------------------------------------------------------
        n_preselect_samples = 5

        true_preselect_count = min(
            n_preselect_samples,
            n_events
        )

        false_preselect_count = min(
            n_preselect_samples,
            n_censored
        )

        true_max_index = true_indices[
            np.argsort(true_values)[::-1][:true_preselect_count]
        ]

        false_max_index = false_indices[
            np.argsort(false_values)[::-1][:false_preselect_count]
        ]

        # --------------------------------------------------------
        # 4. 确定每个基学习器随机抽多少 event/censor
        # --------------------------------------------------------
        true_sample_size = int(
            n_events * current_sample_ratio
        )

        # 保持原来的 event : censor 比例
        false_sample_size = round(
            (true_sample_size + true_preselect_count)
            * n_censored
            / n_events
        ) - false_preselect_count

        true_sample_size = max(
            0,
            true_sample_size
        )

        false_sample_size = max(
            0,
            false_sample_size
        )

        if self.verbose >= 2:
            self.logger.info(
                f"Random event sample size per learner: "
                f"{true_sample_size}"
            )

            self.logger.info(
                f"Random censor sample size per learner: "
                f"{false_sample_size}"
            )

        # --------------------------------------------------------
        # 5. 随机抽样池
        #
        # 如果同一个 learner 内不允许重复：
        # 将固定保留的 5 个 participant 从随机池中移除，
        # 避免随机部分再次抽到他们。
        # --------------------------------------------------------
        if sample_with_replacement:
            true_pool = true_indices
            false_pool = false_indices

        else:
            true_pool = np.setdiff1d(
                true_indices,
                true_max_index
            )

            false_pool = np.setdiff1d(
                false_indices,
                false_max_index
            )

            # 防止抽样数超过剩余 participant 数
            true_sample_size = min(
                true_sample_size,
                len(true_pool)
            )

            false_sample_size = min(
                false_sample_size,
                len(false_pool)
            )

        # --------------------------------------------------------
        # 6. 每个 Cox 基学习器独立抽样
        #
        # 非常重要：
        # 即使 replace=False，也必须每个 learner 单独 np.random.choice。
        #
        # 这样：
        #   Cox 1 内部没有重复
        #   Cox 2 内部没有重复
        # 但 Cox 1 和 Cox 2 可以包含同一个 participant
        # --------------------------------------------------------
        true_sampled_indices = np.vstack([
            np.random.choice(
                true_pool,
                size=true_sample_size,
                replace=sample_with_replacement
            )
            for _ in range(self.n_estimators)
        ])

        false_sampled_indices = np.vstack([
            np.random.choice(
                false_pool,
                size=false_sample_size,
                replace=sample_with_replacement
            )
            for _ in range(self.n_estimators)
        ])

        # --------------------------------------------------------
        # 7. 每个 learner 加回固定的最长随访样本
        # --------------------------------------------------------
        true_preselect_indices = np.repeat(
            true_max_index.reshape(1, -1),
            self.n_estimators,
            axis=0
        )

        false_preselect_indices = np.repeat(
            false_max_index.reshape(1, -1),
            self.n_estimators,
            axis=0
        )

        # --------------------------------------------------------
        # 8. 合并
        # --------------------------------------------------------
        all_sampled_indices = np.hstack((
            true_sampled_indices,
            false_sampled_indices,
            true_preselect_indices,
            false_preselect_indices,
        ))

        return all_sampled_indices

    def generate_valid_indices(self, y):
        # 分为True组和False组
        """Generate out-of-bag validation indices for every base learner."""
        true_group = [(i, val[1]) for i, val in enumerate(y) if val[0] == True]
        false_group = [(i, val[1]) for i, val in enumerate(y) if val[0] == False]

        # 提取两组的索引，以及生存时间
        true_indices = tuple(index for index, _ in true_group)
        false_indices = tuple(index for index, _ in false_group)

        all_valid_indices = []
        for sample_indice in self.sample_indices:   # 遍历每个基学习器的训练样本索引
            oob_true_indices = np.setdiff1d(true_indices, sample_indice)    # 找出不在该训练样本索引里的未删失样本
            oob_false_indices = np.setdiff1d(false_indices, sample_indice)  # 找出不在该训练样本索引里的删失样本

            # 从删失样本中有放回采样与oob_true_indices等量的验证集样本索引
            sel_oob_false_indices = np.random.choice(oob_false_indices, size=len(oob_true_indices), replace=True)
            # 拼接得到最终验证集样本索引
            sel_valid_indices = np.concatenate([oob_true_indices, sel_oob_false_indices])

            # # 2025/5/9更新，新数据中，已经不是删失样本数量远大于未删失样本了，所以这里不用刻意保持删失与未删失验证样本的一致
            # sel_valid_indices = np.concatenate([oob_true_indices, oob_false_indices])

            all_valid_indices.append(sel_valid_indices)

        return all_valid_indices

    def generate_feature_indices(self, x, y):
        """Generate one selected feature subset for each base learner."""
        if self.n_jobs:
            # Multi-process
            pool = mp.Pool(self.n_jobs if self.n_jobs != -1 else mp.cpu_count())
            tasks = [pool.apply_async(self.generate_feature_indices_async,
                                      args=(t_i, x.iloc[self.sample_indices[t_i]], y[self.sample_indices[t_i]]))
                     for t_i in range(self.n_estimators)]
            pool.close()
            pool.join()
            feature_indices = [task.get() for task in tasks]
        else:
            # Single-process
            feature_indices = [self.generate_feature_indices_async(t_i, x.iloc[self.sample_indices[t_i]], y[self.sample_indices[t_i]])
                               for t_i in range(self.n_estimators)]
        return feature_indices

    def generate_feature_indices_async(self, t_i, x, y):
        # 本函数中的一些变量含义：
        # macro feature：在codebook中有记录的原始特征
        # micro feature：除了原始特征外，还包含了通过one-hot编码等构造的新特征

        """Select macro and micro features for one base learner."""
        start = time.time()

        macro_features = list(self.feature_map.keys())

        s = time.time()
        # 计算micro特征两两之间的关联矩阵corr_mat
        use_corr_mat_pre = False
        micro_corr_mat_file = f"{self.tmp_path}/micro_corr_mat_{self.task_id}_{t_i}.csv"
        if use_corr_mat_pre:
            micro_corr_mat = pd.read_csv(micro_corr_mat_file, index_col=0)
        else:
            micro_corr_mat = x.corr(method=fea_corr_type).abs()
            micro_corr_mat.to_csv(micro_corr_mat_file)
        e = time.time()
        if self.verbose >= 2:
            self.logger.info(f"Tree {t_i}, calculate corr_mat for micro features! Time cost: {e-s}")

        s = time.time()
        # 根据micro特征的关联矩阵，计算macro特征的关联矩阵
        use_macro_corr_mat_pre = False   # pre
        macro_corr_mat_file = f"{self.tmp_path}/macro_corr_mat_{self.task_id}_{t_i}.csv"
        if use_macro_corr_mat_pre:
            macro_corr_mat = pd.read_csv(macro_corr_mat_file, index_col=0)
        else:
            n_macro = len(macro_features)
            macro_corr_mat_arr = np.ones((n_macro, n_macro))
            # 预先将所有macro特征对应的micro特征索引存储
            macro_indices = [self.feature_map[m] for m in macro_features]

            for i in range(n_macro):
                macro_corr_mat_arr[i, i] = 1.0
                for j in range(i+1, n_macro):
                    # 提取所有micro特征彼此间的相关性分数
                    sub_mat = micro_corr_mat.loc[macro_indices[i], macro_indices[j]].values

                    # 计算macro特征之间的关联性分数
                    macro_corr_score = operation(sub_mat, macro_r_type)

                    # 将计算好的macro特征的关联性分数存入macro_corr_mat_arr
                    macro_corr_mat_arr[i, j] = macro_corr_mat_arr[j, i] = macro_corr_score

            macro_corr_mat = pd.DataFrame(macro_corr_mat_arr, index=macro_features, columns=macro_features)
            macro_corr_mat.to_csv(macro_corr_mat_file)
        e = time.time()
        if self.verbose >= 2:
            self.logger.info(f"Tree {t_i}, calculate corr_mat for macro features! Time cost: {e-s}")
        del micro_corr_mat

        # 动态选择特征
        if fl_corr_type == 'pearson':
            processes = sele_type.split('+')
            selected_column_s = []
            for process in processes:
                s_p = time.time()

                if process == 'time':
                    y_pro = np.array([val[1] for val in y])
                elif process == 'event':
                    y_pro = np.array([val[0] for val in y])
                else:
                    raise ValueError(f"Unknown process: {process}")

                # 计算macro特征与共病事件、随访时间标签的关联性分数
                use_label_corr_dict_pre = False
                label_r_dict_file = f"{self.tmp_path}/label_r_dict_{self.task_id}_{t_i}_{process}.pkl"
                label_p_dict_file = f"{self.tmp_path}/label_p_dict_{self.task_id}_{t_i}_{process}.pkl"
                if use_label_corr_dict_pre:
                    with open(label_r_dict_file, "rb") as f:
                        label_r_dict = pickle.load(f)
                    with open(label_p_dict_file, "rb") as f:
                        label_p_dict = pickle.load(f)
                else:
                    label_r_dict, label_p_dict = {}, {}
                    for column in macro_features:
                        # 计算macro特征所对应的micro特征们与随访时间标签的关联性分数
                        micro_label_r_s, micro_label_p_s = [], []
                        for col in self.feature_map[column]:
                            r, p = pearsonr(x[col].values, y_pro)
                            micro_label_r_s.append(abs(r))  # 添加r关联的绝对值
                            micro_label_p_s.append(p)       # 添加p-values
                        micro_label_r_s = np.array(micro_label_r_s)
                        micro_label_p_s = np.array(micro_label_p_s)

                        # 得到macro特征与随访时间标签的关联性分数
                        macro_label_r = operation(micro_label_r_s, macro_r_type)

                        # 得到macro特征与随访时间标签的p值
                        macro_label_p = operation(micro_label_p_s, macro_p_type)

                        # 将计算好的macro特征与随访时间标签的关联性分数存入label_corr_dict
                        label_r_dict[column] = macro_label_r
                        label_p_dict[column] = macro_label_p
                    with open(label_r_dict_file, "wb") as f:
                        pickle.dump(label_r_dict, f)
                    with open(label_p_dict_file, "wb") as f:
                        pickle.dump(label_p_dict, f)
                e_p = time.time()
                if self.verbose >= 2:
                    self.logger.info(f"Tree {t_i}, calculate corr between macro features and {process} labels! Time cost: {e_p-s_p}")

                s_p = time.time()
                use_retain_set_pre = False  # pre
                if use_retain_set_pre:
                    with open(f"{self.tmp_path}/retain_set_{self.task_id}_{t_i}_{process}.pkl", "rb") as f:
                        retain_set = pickle.load(f)
                else:
                    retain_set = {}     # 用于记录被保留下来的特征
                    operation_record = f"retain_set is initially empty\n"   # 用于记录特征选择的操作记录

                    # 按macro特征与随访时间标签的关联性分数对macro特征进行排序
                    sorted_features = sorted(label_r_dict, key=label_r_dict.get, reverse=True)
                    for column in sorted_features:  # 遍历macro特征 A
                        operation_record += f"\tConsidering feature [{column}]:\n"

                        # 如果被放弃的特征A，是被事先指定一定要加进来的特征，那么强行将它加入
                        if column in special_remove_features:
                            operation_record += f"\t\tPre-specified remove features! Don't pop it into retain_set\n"
                            continue

                        # 根据macro特征与标签的p值，预过滤高于0.05的特征
                        if label_p_dict[column] > max_fl_corr:
                            operation_record += f"\t\tCorrelation of feature [{column}] with label ({label_p_dict[column]}) higher than threshold {max_fl_corr}! "
                            operation_record += f"Don't pop it into retain_set\n"
                            continue

                        not_corr = True
                        retain_columns = list(retain_set.keys())    # 查看当前选择了哪些特征
                        corr_count = 0
                        for ret_column in retain_columns:   # 遍历当前已选择的特征 B
                            fea_corr = macro_corr_mat.loc[column, ret_column]   # 提取特征A与B之间的相关性分数

                            if fea_corr >= max_ff_corr:  # 如果相关性分数大于阈值，则标明A特征与当前选择的特征之间存在显著相关性
                                not_corr = False
                                corr_count += 1
                                operation_record += f"\t\tSignificant correlation ({fea_corr} >= {max_ff_corr}) with feature [{ret_column}]\n"

                        # 若A特征与当前选择的特征之间存在显著相关性，则不选择特征A
                        if not_corr:
                            operation_record += f"\t\tNo correlated features! Pop it into retain_set\n"
                            retain_set[column] = label_r_dict[column]
                        else:
                            operation_record += f"\t\t{corr_count} correlated features! Don't pop it into retain_set\n"

                        # 如果被放弃的特征A，是被事先指定一定要加进来的特征，那么强行将它加入
                        if column in special_retain_features:
                            operation_record += f"\t\tPre-specified retain features! Directly pop it into retain_set\n"
                            retain_set[column] = 1.1

                    with open(f"{self.ana_path}/operation_record_{self.task_id}_{t_i}_{process}.txt", "w", encoding="utf-8") as file:
                        file.write(operation_record)

                    with open(f"{self.tmp_path}/retain_set_{self.task_id}_{t_i}_{process}.pkl", "wb") as f:
                        pickle.dump(retain_set, f)
                e_p = time.time()
                if self.verbose >= 2:
                    self.logger.info(f"Tree {t_i}, adaptively select features for {process} label! Time cost: {e_p-s_p}")
                del label_r_dict, label_p_dict

                # 选出被保留的macro特征
                if n_fea_sel is not None and n_fea_sel < len(retain_set):
                    # 若指定了要保留多少macro特征（n_fea_sel），则按macro特征与标签的相关性从大到小保留
                    selected_macro_column = [k for k, v in sorted(retain_set.items(), key=lambda item: item[1], reverse=True)[:n_fea_sel]]
                else:
                    selected_macro_column = list(retain_set.keys())

                # 获取被保留的macro特征所对应的micro特征
                selected_column = np.concatenate([self.feature_map[selected_macro_feature] for selected_macro_feature in selected_macro_column])

                selected_column_s.extend(list(selected_column))
            selected_columns = np.array(list(set(selected_column_s)))
            np.savetxt(f"{tmp_dir}/selected_columns_{self.target}_{self.task_id}_{t_i}.csv", selected_columns, fmt='%s', delimiter=',')
        elif fl_corr_type == 'cox':
            # 计算macro特征与标签的关联性分数
            s_p = time.time()
            use_fl_cox_dict_pre = False
            fl_cox_dict_file = f"{self.tmp_path}/fl_cox_dict_{self.task_id}_{t_i}.pkl"
            fl_cox_arr_file = f"{self.tmp_path}/fl_cox_arr_{self.task_id}_{t_i}.csv"
            if use_fl_cox_dict_pre:
                with open(fl_cox_dict_file, "rb") as f:
                    fl_cox_dict = pickle.load(f)
            else:
                if basic_model == 'Cox':
                    model = CoxnetSurvivalAnalysis(
                        fit_baseline_model=True,
                        alpha_min_ratio=(
                            coxnet_alpha_min_ratio_when_n_gt_p
                            if x.shape[0] > x.shape[1] else "auto"
                        ),
                    )
                elif basic_model == 'CoxPH':
                    model = CoxPHSurvivalAnalysis()
                else:
                    raise ValueError(f"Unknown basic_model: {basic_model}!")
                model.fit(x, y)

                if basic_model == 'Cox':
                    coef, _ = model._get_coef(None)
                elif basic_model == 'CoxPH':
                    coef = model.coef_
                else:
                    raise ValueError(f"Unknown basic_model: {basic_model}!")
                fi = np.abs(coef)
                fi /= max(fi)   # 放缩到[0, 1]
                micro_corr_dict = {k: v for k, v in zip(self.feature_names, fi)}
                macro_corr_dict = {macro: np.array([micro_corr_dict[mi] for mi in micro]) for macro, micro in self.feature_map.items()}
                fl_cox_dict = {k: operation(v, macro_c_type) for k, v in macro_corr_dict.items()}
                with open(fl_cox_dict_file, "wb") as f:
                    pickle.dump(fl_cox_dict, f)

                fl_cox_arr = [[k, v] for k, v in fl_cox_dict.items()]
                np.savetxt(fl_cox_arr_file, np.array(fl_cox_arr), fmt='%s', delimiter=',')
            e_p = time.time()
            if self.verbose >= 2:
                self.logger.info(f"Tree {t_i}, calculate corr between macro features and labels using Cox fi! Time cost: {e_p - s_p}")

            s_p = time.time()
            use_retain_set_pre = False  # pre
            retain_set_file = f"{self.tmp_path}/retain_set_cox_{self.task_id}_{t_i}.pkl"
            operation_record_file = f"{self.ana_path}/operation_record_cox_{self.task_id}_{t_i}.txt"
            if use_retain_set_pre:
                with open(retain_set_file, "rb") as f:
                    retain_set = pickle.load(f)
            else:
                special_features = ['sex', 'recruitment_age']  # 这是范师兄要求的必须保留的特征
                retain_set = {}  # 用于记录被保留下来的特征
                operation_record = f"retain_set is initially empty\n"  # 用于记录特征选择的操作记录

                # 按macro特征与标签的关联性分数对macro特征进行排序
                sorted_features = sorted(fl_cox_dict, key=fl_cox_dict.get, reverse=True)
                for column in sorted_features:  # 遍历macro特征 A
                    operation_record += f"\tConsidering feature [{column}]:\n"

                    # 根据macro特征与标签的重要性分数，筛掉关联度小于min_fl_c_corr的特征
                    if fl_cox_dict[column] < min_fl_c_corr:
                        operation_record += f"\t\tCorrelation of feature [{column}] with label ({fl_cox_dict[column]}) lower than threshold {min_fl_c_corr}! "
                        operation_record += f"Don't pop it into retain_set\n"
                        continue

                    not_corr = True
                    retain_columns = list(retain_set.keys())  # 查看当前选择了哪些特征
                    corr_count = 0
                    for ret_column in retain_columns:  # 遍历当前已选择的特征 B
                        fea_corr = macro_corr_mat.loc[column, ret_column]  # 提取特征A与B之间的相关性分数

                        if fea_corr >= max_ff_corr:  # 如果相关性分数大于阈值，则标明A特征与当前选择的特征之间存在显著相关性
                            not_corr = False
                            corr_count += 1
                            operation_record += f"\t\tSignificant correlation ({fea_corr} >= {max_ff_corr}) with feature [{ret_column}]\n"

                    # 若A特征与当前选择的特征之间存在显著相关性，则不选择特征A
                    if not_corr:
                        operation_record += f"\t\tNo correlated features! Pop it into retain_set\n"
                        retain_set[column] = fl_cox_dict[column]
                    else:
                        operation_record += f"\t\t{corr_count} correlated features! Don't pop it into retain_set\n"

                    # 如果被放弃的特征A，是被事先指定一定要加进来的特征，那么强行将它加入
                    if column in special_features:
                        operation_record += f"\t\tPre-specified features! Directly pop it into retain_set\n"
                        retain_set[column] = 1.1

                with open(operation_record_file, "w", encoding="utf-8") as file:
                    file.write(operation_record)

                with open(retain_set_file, "wb") as f:
                    pickle.dump(retain_set, f)
            e_p = time.time()
            if self.verbose >= 2:
                self.logger.info(f"Tree {t_i}, adaptively select features for label! Time cost: {e_p - s_p}")
            del fl_cox_dict

            # 选出被保留的macro特征
            if n_fea_sel is not None and n_fea_sel < len(retain_set):
                # 若指定了要保留多少macro特征（n_fea_sel），则按macro特征与标签的相关性从大到小保留
                selected_macro_column = [k for k, v in sorted(retain_set.items(), key=lambda item: item[1], reverse=True)[:n_fea_sel]]
            else:
                selected_macro_column = list(retain_set.keys())

            # 获取被保留的macro特征所对应的micro特征
            selected_columns = np.concatenate([self.feature_map[selected_macro_feature] for selected_macro_feature in selected_macro_column])
        else:
            raise ValueError(f"Unknown fl_corr_type: {fl_corr_type}")
        del macro_corr_mat

        end = time.time()
        if self.verbose >= 1:
            self.logger.info(f"Tree {t_i}, select {len(selected_columns)}/{x.shape[1]} features! Total time cost: {end-start}")
        return selected_columns

    # 训练基学习器模型（Cox模型）
    def fit_estimator(self, t_i, x, y, x_vld=None, y_vld=None, x_eval=None, y_eval=None):
        # self.logger.info(f"Tree {t_i}, fit model...")
        # 定义基学习器模型
        """Fit or load one Cox base learner and optionally evaluate it."""
        if basic_model == 'Cox':
            model = CoxnetSurvivalAnalysis(
                fit_baseline_model=True,
                alpha_min_ratio=(
                    coxnet_alpha_min_ratio_when_n_gt_p
                    if x.shape[0] > x.shape[1] else "auto"
                ),
            )
        elif basic_model == 'CoxPH':
            model = CoxPHSurvivalAnalysis()
        else:
            raise ValueError(f"Unknown basic_model: {basic_model}!")
        # 拟合基学习器模型
        model.fit(x, y)
        # output_str = f"Tree {t_i} trained"

        model_file = f"{mod_dir}/{self.target}_run_CoxEM_{basic_model}_{n_sample}_{fold_type}_{self.task_id}_{t_i}.pkl" if local_sample \
            else f"{mod_dir}/{self.target}_run_CoxEM_{basic_model}_{fold_type}_{self.task_id}_{t_i}.pkl"
        joblib.dump(model, model_file)

        if self.compute_train_fitness:
            # 预测风险得分 (高风险分数对应更高的事件概率)
            if hasattr(model, 'predict'):
                risks_trn = model.predict(x)
            else:
                raise ValueError("模型不支持风险预测")

            # 计算该基学习器在验证集上的性能，并以此作为权重
            y_t = np.array([val[1] for val in y])
            eval_times = generate_safe_eval_times(y_t, y_t, 365)
            if self.valid_metric == 'auc':
                _, fitness = cumulative_dynamic_auc(y, y, risks_trn, eval_times[2:])
            elif self.valid_metric == 'c-index':
                name_event, name_time = y.dtype.names
                fitness = concordance_index_censored(y[name_event], y[name_time], risks_trn)[0]
            else:
                raise ValueError(f"Unknown valid_metric: {self.valid_metric}!")
        else:
            fitness = -1
        # output_str += f", train {self.valid_metric}: {fitness:.4f}"

        # 使用验证集计算该基学习器模型的权重
        if self.use_valid:
            # self.logger.info(f"Tree {t_i}, calculate weights...")

            # 预测风险得分 (高风险分数对应更高的事件概率)
            if hasattr(model, 'predict'):
                risks_vld = model.predict(x_vld)
            else:
                raise ValueError("模型不支持风险预测")

            y_t = np.array([val[1] for val in y])
            y_vld_t = np.array([val[1] for val in y_vld])
            eval_times = generate_safe_eval_times(y_t, y_vld_t, 365)
            if self.valid_metric == 'auc':
                _, weight = cumulative_dynamic_auc(y, y_vld, risks_vld, eval_times[2:])
            elif self.valid_metric == 'c-index':
                name_event, name_time = y_vld.dtype.names
                weight = concordance_index_censored(y_vld[name_event], y_vld[name_time], risks_vld)[0]
            else:
                raise ValueError(f"Unknown valid_metric: {self.valid_metric}!")
        else:
            weight = 1.
        # output_str += f", valid {self.valid_metric}: {weight:.4f}"

        if self.compute_eval_perf and (x_eval is not None):
            if hasattr(model, 'predict'):
                risks_eval = model.predict(x_eval)
            else:
                raise ValueError("模型不支持风险预测")

            y_t = np.array([val[1] for val in y])
            y_eval_t = np.array([val[1] for val in y_eval])
            eval_times = generate_safe_eval_times(y_t, y_eval_t, 365)

            if self.valid_metric == 'auc':
                _, perf = cumulative_dynamic_auc(y, y_eval, risks_eval, eval_times[2:])
            elif self.valid_metric == 'c-index':
                name_event, name_time = y_eval.dtype.names
                perf = concordance_index_censored(y_eval[name_event], y_eval[name_time], risks_eval)[0]
            else:
                raise ValueError(f"Unknown valid_metric: {self.valid_metric}!")
        else:
            perf = -1
        # output_str += f", test {self.valid_metric}: {perf:.4f}"

        # if self.verbose >= 2:
        #     self.logger.info(output_str)
        return fitness, weight, perf

    def predict(self, x):
        """Predict ensemble risk scores for the supplied feature matrix."""
        predictions = []
        for t_i, model in enumerate(self.estimators):
            if isinstance(x, pd.DataFrame):
                predictions.append(model.predict(x[self.feature_indices[t_i]]) * self.weights[t_i])
            elif isinstance(x, np.ndarray):
                predictions.append(model.predict(x[:, self.feature_indexes[t_i]]) * self.weights[t_i])
            else:
                raise TypeError(f"Unknown type of 'x': {type(x)}")
        return np.sum(np.array(predictions), axis=0)

    def predict_survival_probabilities(self, x, times):
        """Predict weighted ensemble survival probabilities at specified times."""
        times = np.asarray(times, dtype=float)

        if times.ndim != 1 or len(times) == 0:
            raise ValueError("'times' must be a non-empty 1D array.")

        n_samples = x.shape[0]
        survival = np.zeros((n_samples, len(times)), dtype=float)

        for t_i, model in enumerate(self.estimators):
            if isinstance(x, pd.DataFrame):
                x_sub = x.loc[:, self.feature_indices[t_i]]
            elif isinstance(x, np.ndarray):
                x_sub = x[:, self.feature_indexes[t_i]]
            else:
                raise TypeError(f"Unknown type of 'x': {type(x)}")

            if not hasattr(model, "predict_survival_function"):
                raise ValueError(
                    f"Base estimator {t_i} does not support "
                    "survival-function prediction."
                )

            surv_funcs = model.predict_survival_function(x_sub)

            est_survival = np.asarray(
                [
                    [float(fn(t)) for t in times]
                    for fn in surv_funcs
                ],
                dtype=float
            )

            survival += est_survival * float(self.weights[t_i])

        return np.clip(survival, 0.0, 1.0)

    def predict_absolute_risk(self, x, times):
        """Predict absolute event risk as one minus survival probability."""
        return 1.0 - self.predict_survival_probabilities(x, times)

    def get_feature_importances(self):
        """Aggregate weighted coefficients and importances across base learners."""
        feature_importances = np.zeros(self.n_feature)
        beta_save = np.zeros(self.n_feature)
        beta_s = []
        for t_i, model in enumerate(self.estimators):
            if basic_model == 'Cox':
                beta, _ = model._get_coef(None)
            elif basic_model == 'CoxPH':
                beta = model.coef_
            else:
                raise ValueError(f"Unknown basic_model: {basic_model}!")
            fi = np.abs(beta)
            fi_sum = np.sum(fi)
            if np.isfinite(fi_sum) and fi_sum > 0:
                fi = fi / fi_sum
            else:
                fi = np.zeros_like(fi, dtype=float)
            feature_importances[self.feature_indexes[t_i]] += fi * self.weights[t_i]

            beta_save[self.feature_indexes[t_i]] += beta * self.weights[t_i]

            beta_t = np.zeros(self.n_feature)
            beta_t[self.feature_indexes[t_i]] = beta
            beta_s.append(beta_t)

            # coef_save[self.feature_indexes[t_i]] = coef
            # coef_save_file = f"{self.ana_path}/beta_{self.task_id}_{t_i}.pkl"
            # np.savetxt(coef_save_file, np.array([self.feature_names, list(coef_save)]), fmt='%s', delimiter=",")

        return self.feature_names, feature_importances, beta_save, beta_s

    def get_signed_feature_importances(self):
        """Aggregate direction-preserving, L1-normalized feature importance."""
        signed_importances = np.zeros(self.n_feature, dtype=float)

        for t_i, model in enumerate(self.estimators):
            if basic_model == 'Cox':
                beta, _ = model._get_coef(None)
            elif basic_model == 'CoxPH':
                beta = model.coef_
            else:
                raise ValueError(f"Unknown basic_model: {basic_model}!")

            beta = np.asarray(beta, dtype=float)
            beta_l1 = np.sum(np.abs(beta))
            if np.isfinite(beta_l1) and beta_l1 > 0:
                signed_importances[self.feature_indexes[t_i]] += (
                    self.weights[t_i] * beta / beta_l1
                )

        signed_l1 = np.sum(np.abs(signed_importances))
        if np.isfinite(signed_l1) and signed_l1 > 0:
            signed_importances /= signed_l1
        else:
            signed_importances[:] = 0.0

        return self.feature_names, signed_importances


