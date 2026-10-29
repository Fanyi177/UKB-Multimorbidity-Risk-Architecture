#!/usr/bin/env bash
#SBATCH -J UKB_pipeline
#SBATCH -p C064M1024G
#SBATCH --cpus-per-task=64
#SBATCH --time=5-00:00:00
#SBATCH -o UKB_pipeline_%j.out
#SBATCH -e UKB_pipeline_%j.err

set -euo pipefail

# Submit the default experiment or the independent _2 experiment:
#   sbatch run_pipeline_slurm_cn.sh
#   sbatch run_pipeline_slurm_cn.sh --enviro-2
# The default experiment rebuilds derived datasets from completed all
# preprocessing, then runs every model stage without skipping tasks.
# The _2 experiment also fits its own all preprocessing before derivation.
PROJECT_DIR="/lustre/home/2411110208/UK_biobank/code/6.select_and_weight_the_env_vars_of_multimorbidity"
cd "$PROJECT_DIR"

case "${1:-}" in
    "") pipeline_script="run_pipeline_enviro.sh" ;;
    --enviro-2) pipeline_script="run_pipeline_enviro_2.sh" ;;
    *) echo "用法: sbatch run_pipeline_slurm_cn.sh [--enviro-2]" >&2; exit 2 ;;
esac
if [[ $# -gt 1 ]]; then
    echo "用法: sbatch run_pipeline_slurm_cn.sh [--enviro-2]" >&2
    exit 2
fi

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export PYTHONUNBUFFERED=1

echo "========================================"
echo "Pipeline: $pipeline_script"
echo "SLURM Job ID: ${SLURM_JOB_ID:-NA}"
echo "节点: ${SLURMD_NODENAME:-NA}"
echo "申请 CPU 数: ${SLURM_CPUS_PER_TASK:-NA}"
echo "工作目录: $PROJECT_DIR"
echo "Python: $(command -v python)"
echo "开始时间: $(date)"
echo "========================================"

bash "$pipeline_script"

echo "========================================"
echo "Pipeline 完成时间: $(date)"
echo "========================================"
