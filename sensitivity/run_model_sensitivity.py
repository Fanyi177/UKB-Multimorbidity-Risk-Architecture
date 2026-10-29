"""Sensitivity-only Stage 1 CoxEM training.

This file intentionally leaves the primary run_model.py untouched. It writes the
standard fold-level performance/feature evidence required by Stage 2 but does not
serialize the full Stage-1 ensemble model.
"""


import argparse
import pickle
import time
import warnings

import numpy as np
import pandas as pd

from sksurv.datasets import get_x_y
from sksurv.metrics import cumulative_dynamic_auc, concordance_index_censored, integrated_brier_score

from config import (
    CoxEM_n_estimator, CoxEM_n_jobs, ana_dir, basic_model, dat_dir,
    fl_corr_type, fold_type, log_dir, main_verbose, make_dir, metric_names,
    min_fl_c_corr, mod_dir, n_fea_sel, random_state, res_dir, sample_ratio,
    use_valid, valid_metric,
)
from utils import (
    build_stage1_feature_selection_trace, generate_safe_eval_times,
    get_logger, load_processed_fold, read_feature_lineage,
)
from CoxEM import CoxEM

warnings.filterwarnings("ignore")


if __name__ == '__main__':
    # 实验参数
    model_name = "CoxEM"
    parser = argparse.ArgumentParser()
    parser.add_argument("--c_type", type=str, default='comorbidity')
    parser.add_argument("--target", type=str, default='all')
    parser.add_argument("--view", type=str, default='enviro')
    parser.add_argument("--task_id", type=str, default='0')
    args = parser.parse_args()

    c_type = args.c_type
    target = args.target
    view = args.view
    task_id = args.task_id

    dat_path = make_dir(f"{dat_dir}/{c_type}_{target}_{view}")
    if c_type == 'comorbidity' and target == 'all':
        make_dir(f"{mod_dir}/{c_type}_{target}_{view}/"
                 f"sample_ratio={sample_ratio} & event_censor_ratio=fold & "
                 f"fl_corr_type={fl_corr_type} & min_fl_c_corr={min_fl_c_corr} & n_fea_sel={n_fea_sel}")
        res_path = make_dir(f"{res_dir}/{c_type}_{target}_{view}/"
                            f"sample_ratio={sample_ratio} & event_censor_ratio=fold & "
                            f"fl_corr_type={fl_corr_type} & min_fl_c_corr={min_fl_c_corr} & n_fea_sel={n_fea_sel}")
        make_dir(f"{ana_dir}/{c_type}_{target}_{view}/"
                 f"sample_ratio={sample_ratio} & event_censor_ratio=fold "
                 f"& fl_corr_type={fl_corr_type} & min_fl_c_corr={min_fl_c_corr} & n_fea_sel={n_fea_sel}")
    else:
        make_dir(f"{mod_dir}/{c_type}_{target}_{view}")
        res_path = make_dir(f"{res_dir}/{c_type}_{target}_{view}")
        make_dir(f"{ana_dir}/{c_type}_{target}_{view}")

    # 实验日志
    LOGGER = get_logger(f"main_{c_type}_{target}_{view}_{fold_type}", log_dir)
    # 实验可复现性
    np.random.seed(random_state)

    # 实验开始
    start = time.time()

    feature_map_file = f"{dat_path}/feature_map_{fold_type}_{task_id}.pkl"
    with open(feature_map_file, 'rb') as f:
        feature_map = pickle.load(f)

    # 读取实验数据
    LOGGER.info(f"Model {model_name}_{basic_model}, Comorbidity {c_type}, Target {target}, View {view}, Task {task_id}, load data...")
    train_data, valid_data = load_processed_fold(
        dat_path, fold_type, task_id
    )
    eval_times = generate_safe_eval_times(train_data['time'], valid_data['time'], 365)

    # 获取能够被生存预测模型处理的x和y
    attr_labels = ['status', 'time']
    train_data_model = train_data.drop(columns=['eid'], errors='ignore')
    valid_data_model = valid_data.drop(columns=['eid'], errors='ignore')

    x_train, y_train = get_x_y(train_data_model, attr_labels, 1, True)
    x_valid, y_valid = get_x_y(valid_data_model, attr_labels, 1, True)
    name_event, name_time = y_valid.dtype.names

    # 模型定义及训练
    LOGGER.info(f"Model {model_name}_{basic_model}, Task {task_id}, training...")
    # Sensitivity-only Stage 1: use a globally unique CoxEM task id so
    # temporary/intermediate files cannot collide across outcomes/views/folds.
    # External Stage-1 result filenames still use the original numeric task_id.
    coxem_task_id = f"{c_type}__{target}__{view}__fold{task_id}"
    model = CoxEM(
        c_type, target, coxem_task_id,
        CoxEM_n_estimator, random_state, CoxEM_n_jobs,
        feature_map, use_valid, valid_metric, main_verbose
    )
    model.fit(x_train, y_train, x_valid, y_valid)

    # IMPORTANT: do not serialize the complete Stage-1 CoxEM ensemble for
    # sensitivity analyses. Downstream Stage 2 uses the saved performance,
    # feature-importance, signed-importance, beta and SE files instead.

    # 预测风险得分 (高风险分数对应更高的事件概率)
    if hasattr(model, 'predict'):
        risks_valid = model.predict(x_valid)
    else:
        raise ValueError("模型不支持风险预测")

    # 性能评估
    LOGGER.info(f"Model {model_name}_{basic_model}, Task {task_id}, evaluating...")
    perf_s, perf_str = [], ''

    # 计算auc
    if 'auc' in metric_names:
        # 使用cumulative_dynamic_auc计算时间依赖性AUC
        auc_values, _ = cumulative_dynamic_auc(y_train, y_valid, risks_valid, eval_times)
        _, mean_auc = cumulative_dynamic_auc(y_train, y_valid, risks_valid, eval_times[2:])

        auc_csv_file = f"{res_path}/{model_name}_{basic_model}_AUC_{fold_type}_{task_id}.csv"
        np.savetxt(auc_csv_file,
                   np.array(np.hstack((np.array([['time', 'AUC']]).T, np.array([eval_times, auc_values])))),
                   fmt='%s', delimiter=',')

        perf_s.append(mean_auc)
        perf_str += f"auc: {mean_auc:.4f} "
    # 计算c-index
    if 'c-index' in metric_names:
        c_index = concordance_index_censored(y_valid[name_event], y_valid[name_time], risks_valid)[0] # 注意取负值使其方向正确（高风险→低生存）

        perf_s.append(c_index)
        perf_str += f"c-index: {c_index:.4f} "
    # 计算ibs
    if 'ibs' in metric_names:
        surv_funcs, weights = model.predict_survival_function(x_valid)
        preds = np.sum(np.array([[[fn(t) * weight for t in eval_times] for fn in surv_func] for surv_func, weight in
                                 zip(surv_funcs, weights)]), axis=0)
        ibs = integrated_brier_score(y_train, y_valid, preds, eval_times)

        perf_s.append(ibs)
        perf_str += f"ibs: {ibs:.4f} "
    # 计算time
    if 'time' in metric_names:
        end = time.time()
        cost_time = (end - start) / 60.
        perf_s.append(cost_time)
        perf_str += f"time: {cost_time:.4f}(min) "
    # 输出模型性能
    LOGGER.info(f"Model {model_name}_{basic_model}, Task {task_id} perf: " + perf_str)
    # 存储实验结果
    perf_file = f"{res_path}/{model_name}_{basic_model}_perf_{fold_type}_{task_id}.csv"
    save_data = np.vstack((np.array([metric_names]), np.array([perf_s])))
    np.savetxt(perf_file, save_data, fmt='%s', delimiter=',')

    # 存储特征重要性分数
    feature_names, fi, beta, beta_s = model.get_feature_importances()

    preprocess_lineage = read_feature_lineage(
        f"{dat_path}/feature_lineage_preprocess.csv"
    )
    stage1_trace = build_stage1_feature_selection_trace(
        lineage=preprocess_lineage,
        feature_names=feature_names,
        feature_importances=fi,
        feature_indices=model.feature_indices,
        view=view,
    )
    stage1_trace.to_csv(
        f"{res_path}/feature_selection_stage1_{fold_type}_{task_id}.csv",
        index=False,
    )

    fi_file = f"{res_path}/{model_name}_{basic_model}_raw_fi_{fold_type}_{task_id}.csv"
    np.savetxt(fi_file, np.array([feature_names, list(fi)]), fmt='%s', delimiter='\t')

    beta_file = f"{res_path}/{model_name}_{basic_model}_raw_beta_{fold_type}_{task_id}.csv"
    np.savetxt(beta_file, np.array([feature_names, list(beta)]), fmt='%s', delimiter='\t')

    signed_feature_names, signed_fi = model.get_signed_feature_importances()
    signed_fi_file = (
        f"{res_path}/{model_name}_{basic_model}_raw_signed_fi_"
        f"{fold_type}_{task_id}.csv"
    )
    np.savetxt(
        signed_fi_file,
        np.array([signed_feature_names, list(signed_fi)]),
        fmt='%s',
        delimiter='\t'
    )

    se = np.std(beta_s, axis=0, ddof=1)
    se_file = f"{res_path}/{model_name}_{basic_model}_raw_se_{fold_type}_{task_id}.csv"
    np.savetxt(se_file, np.array([feature_names, list(se)]), fmt='%s', delimiter='\t')
