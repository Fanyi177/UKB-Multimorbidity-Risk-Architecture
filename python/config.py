"""Define project paths and fixed experiment configuration."""

# config.py
import os

# Set UKB_USE_ENVIRO_2=1 to run the complete alternative-environment
# pipeline alongside the default pipeline without sharing output directories.
use_enviro_2 = os.environ.get('UKB_USE_ENVIRO_2', '0') == '1'
experiment_suffix = '_2' if use_enviro_2 else ''

# 定义 make_dir 函数
def make_dir(path):
    """Create a directory when needed and return its path."""
    os.makedirs(path, exist_ok=True)
    return path
    
### 路径配置
src_dir = make_dir('Source')
# 文件名
desc_name = f'UKB_variable_coding.xlsx'      # Coding book
desc_name_new = f'UKB_variable_coding.csv'      # Coding book
# 这些数据还是要放在Source文件夹下
feat_name_dict = {
    'enviro': "final_environment_2.csv" if use_enviro_2 else "final_environment.csv",
    'metabolomics': "final_metabolomics.csv",
    'biomarkers': "final_biomarkers.csv",
    'gene': "final_genetics.csv",
}
spec_name = f"final_eid_sex.csv"
label_name = f'UKB_diseases_data_with_patterns.csv'           # UKB数据
dat_dir = make_dir(f'Data{experiment_suffix}')
log_dir = make_dir(f'Log{experiment_suffix}')
res_dir = make_dir(f'Result{experiment_suffix}')
ana_dir = make_dir(f'Analysis{experiment_suffix}')
mod_dir = make_dir(f'Model{experiment_suffix}')
tmp_dir = make_dir(f'Tmp{experiment_suffix}')


### 实验配置
# 随机数
random_state = 66   # 随机数种子

# 数据划分
fold_type = 'rskf'   # 取值及含义：rskf(多次分层k折交叉验证), rkf(多次k折交叉验证), tts(多次训练集-测试集划分)
n_splits = 10        # rkf时表示k折为几折，tts时表示测试集占总数据量的比例为1/n_splits
n_repeats = 1       # rkf时表示进行多少次‘k折交叉验证’，tts时表示进行多少次‘训练集-测试集划分’

# Cross-validation task count
n_process = n_repeats * n_splits if fold_type != 'tts' else n_repeats   # 计算总共需要的进程数(每个进程处理一折实验，所有进程同时进行实验，节省时间开销)


# data_preprocess.py
local_sample = False    # 是否只采用部分数据进行预实验
n_sample = 5000         # 若是，则采用多少个样本进行预实验
balance_event = True    # 若是，在采样时是否保证1和0类样本一致
# norm_type = 'norm_min_max'      # 使用何种方法对连续型变量进行标准化处理：None, norm, tanh, norm_tanh, min_max。其中None是不进行处理。
#fill_type = 'simple_mean'   # 使用何种方法对连续型变量进行缺失值处理：simple_mean, simple_median, iterative。其中simple_mean是用均值进行填充，simple_median是用中位数进行填充。
couple_feature = True   # 是否对关联特征进行耦合处理
global_missing_threshold = 0.8


# feature_select
sele_type = 'event'      # 基于何种度量对特征进行选择：time，event，event+time。其中event+time是分别基于event和time各选择n_fea_sel个特征，然后合并并去掉重复特征。

fea_corr_type = 'pearson'
macro_r_type = 'max' # mean, max, min   # 如何将micro转化为macro，对应pearson的r值
macro_p_type = 'min' # mean, max, min   # 如何将micro转化为macro，对应pearson的p值
macro_c_type = 'max' # mean, max, min   # 如何将micro转化为macro，对应cox的fi值
max_ff_corr = 0.9      # 特征与特征的关联度阈值（r值超过该值则移除）
max_fl_corr = 0.05      # 特征与标签的关联度阈值（p值高于该值则移除）
min_fl_c_corr = 0.       # 特征与标签的关联度阈值（cox的fi值低于该值则移除）  # 这个可以调参
n_fea_sel = 300         # 进行特征选择时，保留多少个特征

# ============================================================
# CoxEM 基学习器抽样参数
# ============================================================

# 普通结局：每个 Cox 基学习器抽取 10%
sample_ratio = 0.10

# 少事件结局：提高每个 Cox 使用的样本比例
rare_event_sample_ratio = 0.30

# 判断是否属于少事件结局：
# 注意这里指的是“当前进入 CoxEM.fit() 的训练数据中的事件数”，
# 不是全队列的总事件数
rare_event_threshold = 600

# True:
#   同一个 Cox 基学习器内部允许同一个 participant 被重复抽到
# False:
#   同一个 Cox 内不重复；
#   但不同 Cox 基学习器之间仍然可以抽到同一个 participant
#
# 推荐 False，尤其是少事件模式，用尽量多的不同阳性病例
sample_with_replacement = False

basic_model = 'Cox'     # Cox, CoxPH    CoxEM的基础模型
fl_corr_type = 'cox'    # 根据cox还是pearson来评估特征与标签的关联

special_retain_features = ['sex', 'recruitment_age']    # 这是范师兄要求的必须保留的特征
special_remove_features = []    # 这是范师兄要求的必须删除的特征


# 训练过程可视化参数
main_verbose = 2

# 模型参数
CoxEM_n_estimator = 10
CoxEM_n_jobs = -1
# Raise the Coxnet path endpoint only when its automatic ratio would be 1e-4.
coxnet_alpha_min_ratio_when_n_gt_p = 1e-3
use_valid = True
valid_metric = 'auc'
show_fitness = True     # 展示每个基学习器的拟合情况
show_perf = True        # 展示每个基学习器的性能

# 评估指标
metric_names = ['auc', 'c-index', 'time']


# 共病模式，不想要哪个就把该行注释掉
comorbidity_types = [
    'comorbidity',
    'comorbidity_pattern_1',
    'comorbidity_pattern_2',
    'comorbidity_pattern_3',
    'comorbidity_pattern_4',
    'comorbidity_pattern_5',
    'comorbidity_pattern_6',
    'comorbidity_pattern_7',
    'comorbidity_pattern_8',
]
# 不用改
comorbidity_labels = {
    'comorbidity': ['comorbidity', 'follow_time'],
    'comorbidity_pattern_1': ['comorbidity_pattern_1', 'comorbidity_pattern_1_follow_time'],
    'comorbidity_pattern_2': ['comorbidity_pattern_2', 'comorbidity_pattern_2_follow_time'],
    'comorbidity_pattern_3': ['comorbidity_pattern_3', 'comorbidity_pattern_3_follow_time'],
    'comorbidity_pattern_4': ['comorbidity_pattern_4', 'comorbidity_pattern_4_follow_time'],
    'comorbidity_pattern_5': ['comorbidity_pattern_5', 'comorbidity_pattern_5_follow_time'],
    'comorbidity_pattern_6': ['comorbidity_pattern_6', 'comorbidity_pattern_6_follow_time'],
    'comorbidity_pattern_7': ['comorbidity_pattern_7', 'comorbidity_pattern_7_follow_time'],
    'comorbidity_pattern_8': ['comorbidity_pattern_8', 'comorbidity_pattern_8_follow_time'],
}
comorbidity_targets = {
    'comorbidity': ['all', 
    'male', 
    'female'
    ],   # 只有原始共病跑三种数据
    'comorbidity_pattern_1': ['all'],
    'comorbidity_pattern_2': ['all'],
    'comorbidity_pattern_3': ['all'],
    'comorbidity_pattern_4': ['all'],
    'comorbidity_pattern_5': ['all'],
    'comorbidity_pattern_6': ['all'],
    'comorbidity_pattern_7': ['all'],
    'comorbidity_pattern_8': ['all'],
}

# 提前保留时间最大的几个样本
top_k_survival_samples = 5

# ===== 独立外部测试集 =====
independent_test_size = 0.4   # 6:4 划分

# ===== 预处理策略 =====
# E, B, M and G are aligned by eid and enter one development-fitted imputer.
preprocess_impute_method = "iterative_rf"   # iterative_rf / simple
preprocess_add_missing_indicator = False
preprocess_scale_order = "minmax_then_zscore"   # 连续变量：先缩放再标化

# IterativeImputer(RandomForest) parameters; median is initialization only.
iterative_max_iter = 3
iterative_tol = 0.0
iterative_n_estimators = 50
iterative_max_depth = 6
iterative_n_jobs = -1
iterative_n_nearest_features = 300
iterative_skip_complete = True
iterative_verbose = 2
iterative_progress_every = 10
# ===== Stage 2 parallelization =====

# 10-fold CV，因此最多同时跑 10 个 fold
stage2_parallel_folds = 10

# 非常重要：
# 外层 fold 已经并行，因此 Stage 2 内部 CoxEM 不再开多进程
# None 会让 CoxEM 内部按顺序跑，避免 nested multiprocessing
stage2_coxem_n_jobs = None

# ===== Stage 2 Top-N selection =====

# 主选择指标
stage2_metric_to_select_n = "auc"

# Top 10, 20, 30, ...
num_features_per_step = 10

# 连续多少次增益很小后提前停止
stage2_patience = 3

# AUC 提升 <= 0.001 视为没有实质提升
stage2_min_improvement = 0.001

# 至少要求多少个内部 CV folds 成功
# 当前是 10-fold，因此建议至少 8 个成功
stage2_min_successful_folds = 8

# 最终采用 1-SE rule：
# 在 best mean AUC - SE(best) 范围内选择最少变量
stage2_selection_rule = "1se"


# ===== Stage 3 final prediction =====
# Stage 2 final features -> all-view union -> final CoxEM on 60% development
# -> one-time evaluation on the 40% held-out set.

# Current completed Stage 2 outputs are under Result/Stage 2/.
# Change to "stage_parallel" only if you decide to use that rerun instead.
stage3_stage2_result_folder = "Stage 2"

stage3_result_folder = "Stage 3"

stage3_primary_views = [
    "enviro",
    "metabolomics",
    "biomarkers",
    "gene",
]

# Parallelize the 10 CoxEM base learners within each final model.
stage3_coxem_n_jobs = 10

# Do not re-select features inside CoxEM in Stage 3.
stage3_fixed_feature_set = True

# Fixed prediction horizons (follow-up time is in days).
stage3_prediction_horizons_days = [365 * 5, 365 * 10]

# Calibration groups on the held-out set.
stage3_calibration_bins = 10

# Training-derived risk-score reference quantiles.
stage3_risk_quantiles = [0.25, 0.50, 0.75, 0.80, 0.90]

stage3_save_final_model = True
stage3_compute_ibs = True
