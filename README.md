# Forecasting
외부 요인을 고려한 demand &amp; brand share forecasting

# SARIMAX 외생 변수 최적화 방법론 비교
이 프로젝트는 SARIMAX 모델의 외생 변수(exogenous variables)를 선정하는 두 가지 방법론, **Elastic-Net**과 **주성분 분석(PCA)**을 비교하고 성능을 평가하는 R 파이프라인



## 📂 프로젝트 구조

- **`utils.R`**: 두 파이프라인이 공통으로 사용하는 헬퍼 함수 모음
- **`pipeline_elasticnet.R`**: Elastic-Net 기반의 변수 선택 및 예측 파이프라인
- **`pipeline_pca.R`**: PCA 기반의 변수 축소 및 예측 파이프라인
- **`run_comparison.R`**: **두 파이프라인을 모두 실행하고 결과를 비교하는 메인 스크립트**
- **`data/sample_data.xlsx`**: 테스트용 샘플 데이터

## 🚀 실행 방법

1.  **필요 패키지 설치**
    - `utils.R` 파일 상단의 `libs` 벡터에 있는 모든 패키지를 설치

2.  **비교 파이프라인 실행**
[📥 sampledata.xlsx](https://github.com/jay-lay-down/demand_forecasting/raw/main/data/sampledata.xlsx)
- **`elasticnetforecast 패키지 테스트
```r
install.packages("remotes")
remotes::install_github("jay-lay-down/demand_forecasting", subdir = "regforecast", upgrade = "never")
library(regforecast)

res <- run_pipeline(
  file_path      = "파일경로",
  pg_name        = "Snack",      
  sheet_name     = "data",       
  forecast_h     = 12,
  test_end       = c(2024, 12),
  sw_thr         = 0.05,
  backtest_start = c(2023, 1)
)
```

- **`pcaforecast 패키지 테스트
```r
install.packages("remotes")
remotes::install_github("jay-lay-down/demand_forecasting", subdir = "regforecast", upgrade = "never")
library(regforecast)

res <- run_pipeline(
  file_path      = "파일경로",
  pg_name        = "Snack",      
  sheet_name     = "data",       
  forecast_h     = 12,
  test_end       = c(2024, 12),
  sw_thr         = 0.05,
  backtest_start = c(2023, 1)
)
```



3.  **결과 확인**
    - 스크립트 실행이 완료되면 콘솔에 두 방법론의 최종 정확도 비교 테이블이 출력
    - 프로젝트 폴더에 각 방법론의 상세 결과가 담긴 `Predicted_..._elasticnet.xlsx` 파일과 `Predicted_..._PCA.xlsx` 파일이 생성
      
## ✔️​ 결과 예시



