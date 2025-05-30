# detect_file_encoding --------------------------------------------------------

#' 파일의 문자 인코딩을 추정
#'
#' @param file_path 대상 파일 경로
#' @param n 샘플 바이트 수 (기본: 10000)
#' @return 리스트 (encoding, confidence)
#' @export
detect_file_encoding <- function(file_path, n = 10000) {
  raw <- readBin(file_path, what = "raw", n = n)
  guess <- stringi::stri_enc_detect(raw)[[1]]
  enc <- guess$Encoding[1]
  conf <- guess$Confidence[1]
  
  flog.info("인코딩 추정: %s (신뢰도: %.2f) - %s", enc, conf, basename(file_path))
  return(list(encoding = enc, confidence = conf))
}

# csv_loader_keyword ----------------------------------------------------------

#' 특정 키워드를 포함하는 CSV 파일을 모두 로드 (모든 컬럼을 문자열로 읽기)
#'
#' @param keyword 파일명에 포함된 키워드 (예: "개설현황")
#' @param path    폴더 경로 (기본: "data")
#'
#' @return list(file = ..., encoding = ..., confidence = ..., data = ...) 구조 리스트
#' @export
csv_loader_keyword <- function(keyword, path = "data") {
  files <- list.files(
    path = path,
    pattern = paste0(keyword, ".*\\.csv$"),
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    flog.warn("❌ 해당 키워드를 포함하는 CSV 파일이 없습니다: '%s'", keyword)
    return(NULL)
  }
  
  flog.info("📂 총 %d개의 파일을 처리합니다: %s", length(files), paste(basename(files), collapse = ", "))
  
  results <- lapply(files, function(f) {
    flog.info("▶ 파일 시작: %s", basename(f))
    
    enc_info <- detect_file_encoding(f)
    
    raw_lines <- tryCatch(
      readLines(f, encoding = enc_info$encoding, warn = FALSE),
      error = function(e) {
        flog.error("🚫 readLines 실패: %s - %s", basename(f), e$message)
        return(NULL)
      }
    )
    
    if (is.null(raw_lines)) {
      return(list(
        file = basename(f),
        encoding = enc_info$encoding,
        confidence = enc_info$confidence,
        data = NULL
      ))
    }
    
    utf8_lines <- iconv(raw_lines, from = enc_info$encoding, to = "UTF-8", sub = "byte")
    
    dt <- tryCatch(
      data.table::fread(
        text = paste(utf8_lines, collapse = "\n"),
        encoding = "UTF-8",
        colClasses = "character"
      ),
      error = function(e) {
        flog.error("🚫 fread 실패: %s - %s", basename(f), e$message)
        return(NULL)
      }
    )
    
    flog.info("✅ %s: %d행 %d열", basename(f), nrow(dt), ncol(dt))
    
    list(
      file       = basename(f),
      encoding   = enc_info$encoding,
      confidence = enc_info$confidence,
      data       = dt
    )
  })
  
  return(results)
}
