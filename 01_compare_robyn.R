# 作業ディレクト
setwd("./dev/MMM_03") # 今回の作業フォルダ前回は MMM_01

install.packages("Robyn") # install.packagesに変更
# remotes　インストール
# install.packages('remotes')
# install 
# remotes::install_github("facebookexperimental/Robyn/R")

# reticulate
install.packages("reticulate") # install.packagesは最初の一回のみでOK
library(reticulate)
# dplyr インストール
install.packages("dplyr") 
library(dplyr)
# Robyn
library(Robyn)
packageVersion("Robyn")

# pythonの環境を準備
virtualenv_create("r-reticulate")
py_install("nevergrad", pip = TRUE)
use_virtualenv("r-reticulate", required = TRUE)

use_python("~/.virtualenvs/r-reticulate/bin/python") # macではシェル起動時のディレクトリにある

################################################################
#### Step 1: データ読み込み
dt_simulated = read.csv(file='data.csv')
# dt_simulated = read.csv(file='sample.csv')
# dt_simulated = dplyr::select(dt_simulated,datetime=1, Net.Spend=2 ,
#                              Tv.Spend=3 ,temperature=4,
#                              rain=5, revenue=6, Weekend.FLG=7,dplyr::everything())
#head(dt_simulated)
#data("dt_simulated_weekly")
# head("dt_simulated_weekly")
data("dt_prophet_holidays")
head(dt_prophet_holidays)

#オブジェクト保存用のフォルダ指定 (ご自身の環境名に変更)
robyn_object <- "/Users/mitsuru_urushibata/dev/MMM_03/MyRobyn.RDS"

################################################################
#### Step 2a: モデル構築

#### 2a-1: 入力変数の指定
#input 
InputCollect <- robyn_inputs(
  dt_input = dt_simulated #入力する元データ
  ,dt_holidays = dt_prophet_holidays #祝日データ
  ,date_var = "Date" # 以下のフォーマットで"2020-01-01"
  ,dep_var = "Sales" # 売上高やCVなどの従属変数
  ,dep_var_type = "revenue" # 売上高かCVフラグか
  ,prophet_vars = c("trend", "season") # "trend","season", "weekday" & "holiday"
  ,prophet_country = "US"# 国名 祝日 デフォルトで日本がないため一旦USとしておく
  # ,context_vars = c("temperature", "rain","Weekend.FLG") # イベント情報
  ,paid_media_spends = c("TV","Radio","Banners") # メディア投下
  ,paid_media_vars = c("TV","Radio","Banners")  # メディア
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  # ,organic_vars = c("newsletter") # PRなどの非広告メディア
  # ,factor_vars = c("Weekend.FLG") # イベント因子
  ,window_start = "2018-01-07" # モデル構築に使用するデータの開始日
  ,window_end = "2021-10-31" # モデル構築に使用するデータの終了日
  ,adstock = "geometric" # adstockの形状
)
print(InputCollect)

#### 2a-2: ハイパーパラメータの定義

# 3.6.0からの追加 変数名変換のおまじない?
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

#上記のハイパーパラメータの可変領域を設定
hyperparameters <- list(
  TV_alphas = c(0.5, 3)
  ,TV_gammas = c(0.3, 1)
  ,TV_thetas = c(0, 0.3)
  
  ,Radio_alphas = c(0.5, 3)
  ,Radio_gammas = c(0.3, 1)
  ,Radio_thetas = c(0.1, 0.4)
  
  ,Banners_alphas = c(0.5, 3)
  ,Banners_gammas = c(0.3, 1)
  ,Banners_thetas = c(0.1, 0.4)
)

#### 2a-3: Third, ハイパーパラメータをrobyn_inputs()に入力

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)

if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}

################################################################
#### Step 3: モデル構築

OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to max available - 1
  iterations = 2000, # 2000 recommended for the dummy dataset with no calibration
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = FALSE, # 3-way-split time series for NRMSE validation.# 追加
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)
print(OutputModels)

## パレート最適解を計算
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = 1, # ここはマニュアル通りではダメ automatically pick how many pareto-fronts to fill min_candidates
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_pareto = TRUE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = robyn_object, # path for plots export
  export = TRUE # 追加機能 this will create files locally
)
print(OutputCollect)

################################################################
#### Step 4: モデルを選択

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "2_500_4" # Pick one of the models from OutputCollect to proceed

#### Since 3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model)
print(ExportedModel)

################################################################
#### Step 5: Get budget allocation based on the selected model above
AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_historical_response",
  channel_constr_low = c(0.01, 0.01, 0.01), #メディア数と同じ長さが必要
  channel_constr_up = c(10, 10, 10),
  export = TRUE,
  date_min = "2018-01-07",
  date_max = "2021-10-31"
)
print(AllocatorCollect1)

# Run the "max_response_expected_spend" scenario: "What's the maximum response for a given
# total spend based on historical saturation and what is the spend mix?" "optmSpendShareUnit"
# is the optimum spend share.
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = c(0, 0, 0),
  channel_constr_up = c(4, 4, 4),
  expected_spend = 290177, # Total spend to be simulated
  expected_spend_days = 44*7, # Duration of expected_spend in days
  export = TRUE
)
print(AllocatorCollect2)
AllocatorCollect2$dt_optimOut

