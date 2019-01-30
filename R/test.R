.get_process_name <- function() {
  ps::ps_handle() %>%
    ps::ps_exe()
}

#'@importFrom magrittr %>%
#'@importFrom magrittr extract
#'@importFrom magrittr extract2
#'@export
test_course <- function(course.dir, lesson.name, repos = getOption("repos"), answer.yaml = NULL) {
  .env <- Sys.getenv()
  .env[["R_LIBS"]] <- paste(.libPaths(), collapse = ":")
  for(category in c(
    "LC_COLLATE", "LC_CTYPE",
    "LC_MONETARY", "LC_NUMERIC", "LC_TIME", "LC_MESSAGES",
    "LC_PAPER", "LC_MEASUREMENT")
  ) {
    .env[[category]] <- Sys.getlocale(category = category)
  }
  .env[["SWIRL_DEV"]] <- "TRUE"
  if (file.exists(file.path(R.home("bin"), "R"))) {
    p <- subprocess::spawn_process(
      file.path(R.home("bin"), "R"),
      c("--no-save", "--no-readline", "--quiet", "--interactive"),
      .env,
      workdir = getwd()
    )
  } else {
    p <- subprocess::spawn_process(
      file.path(R.home("bin"), "R.exe"),
      c("--no-save", "--ess"),
      .env,
      workdir = getwd()
    )
  }
  p.buf <- new.env()
  p.buf$output <- list()
  p.buf$show <- 0

  get_stdout <- function() {
    lapply(p.buf$output, "[[", "stdout") %>%
      unlist()
  }

  get_stderr <- function() {
    lapply(p.buf$output, "[[", "stderr") %>%
      unlist()
  }

  get_buf_size <- function() {
    length(p.buf$output)
  }

  search_output <- function(checker, is.output = TRUE) {
    lapply(p.buf$output, "[[", ifelse(is.output, "stdout", "stderr")) %>%
      sapply(checker) %>%
      which()
  }

  get_stderr <- function() {
    lapply(p.buf$output, "[[", "stderr") %>%
      unlist()
  }

  show <- function() {
    if (p.buf$show < length(p.buf$output)) {
      p.buf$show <- p.buf$show + 1
      cat(sprintf("(%d)---stdout---\n", p.buf$show))
      cat(p.buf$output[[p.buf$show]]$stdout, sep = "\n")
      cat(sprintf("(%d)---stderr---\n", p.buf$show))
      cat(p.buf$output[[p.buf$show]]$stderr, sep = "\n")
    }
  }

  read <- function() {
    p.buf$output[[length(p.buf$output) + 1]] <- subprocess::process_read(p)
    show()
  }

  wait_until <- function(checker, is.stdout = TRUE, check.last = TRUE, current.index = get_buf_size()) {
    if (check.last && current.index > 0) {
      if (is.stdout) {
        if (checker(p.buf$output[[current.index]]$stdout)) return(invisible(NULL))
      } else {
        if (checker(p.buf$output[[current.index]]$stderr)) return(invisible(NULL))
      }
    }
    retry <- 0
    colddown <- 0.1
    start.index <- current.index + 1
    while(TRUE) {
      Sys.sleep(colddown)
      read()
      end.index <- get_buf_size()
      for(i in start.index:end.index) {
        if (is.stdout) {
          if (checker(p.buf$output[[i]]$stdout)) return(invisible(NULL))
        } else {
          if (checker(p.buf$output[[i]]$stderr)) return(invisible(NULL))
        }
      }
      start.index <- end.index + 1
      retry <- retry + 1
      if (retry %% 5 == 0) enter_process("\n")
      if (retry > 600) stop(sprintf("wait_until timeout"))
      colddown <- min(colddown + 0.1, 1)
    }
  }

  search_selection <- function(txt, ans) {
    for(char in c("\\", "(", ")", "^", "[", "]", "{", "}", ".", "$", "*", "+")) {
      ans <- gsub(char, sprintf("\\%s", char), ans, fixed = TRUE)
    }
    . <- regexec(sprintf("^\\s*(\\d+): %s$", ans), txt) %>%
      regmatches(x = txt) %>%
      Filter(f = function(.) length(.) == 2)
    if(length(.) != 1) {
      cat("search_selection error:\n", file = stderr())
      cat("\n\n==txt==\n\n", file = stderr())
      cat(txt, sep = "\n", file = stderr())
      cat("\n\n==ans==\n\n", file = stderr())
      cat(ans, sep = "\n", file = stderr())
      stop("")
    }
    .[[1]][2]
  }

  enter_process <- function(cmd, breakline = TRUE) {
    if (breakline) {
      if (substring(cmd, nchar(cmd), nchar(cmd)) != "\n") {
        cmd <- sprintf("%s\n", cmd)
      }
    }
    subprocess::process_write(p, cmd)
    cat(sprintf("(process_write)> %s\n", cmd))
    Sys.sleep(0.1)
    read()
    show()
    invisible(NULL)
  }

  get_character <- function(expr.txt) {
    cmd <- sprintf("cat(sprintf('output:%%s:\\n', %s))", expr.txt)
    enter_process(cmd, breakline = TRUE)
    wait_until(function(.) any(grepl("output:", ., fixed = TRUE)))
    . <- lapply(p.buf$output, "[[", "stdout") %>%
      grep(pattern = "output:", fixed = TRUE) %>%
      max()
    . <- p.buf$output[[.]]$stdout
    m <- regexec("output:(.*):$", .)
    regmatches(., m) %>%
      Filter(f = function(.) length(.) == 2) %>%
      extract2(1) %>%
      extract(2)
  }

  enter_swirl <- function() {
    enter_process(sprintf("options(repos=c(CRAN='%s'))\n", repos))
    enter_process("options(editor = function(...){}, browser = function(...){})\n")
    enter_process(". <- as.environment('package:utils')")
    enter_process("unlockBinding('View', .)")
    enter_process("assign('View', function(x, title) NULL, envir = .)")
    enter_process("lockBinding('View', .)")
    enter_process("library(swirl)")
    enter_process("swirl::delete_progress('wush')\n")
    enter_process("swirl::uninstall_all_courses(force = TRUE)")
    enter_process(sprintf("swirl::install_course_directory(path = '%s')", course.dir))
    enter_process('dir.create(file.path(system.file("", package = "swirl"), "user_data", "wush"), recursive = TRUE)')
    enter_process("assign('wush', '333', swirl:::.swirl_classroom_auth_cache)")
    enter_process("swirl()\n")
    enter_process("3\n")
    enter_process("wush\n")
    enter_process("\n")
  }

  enter_course <- function(name) {
    wait_until(function(.) any(grepl("帶我去 swirl 課程庫！", ., fixed = TRUE)), check.last = TRUE)
    . <- search_output(function(.) any(grepl("帶我去 swirl 課程庫！", ., fixed = TRUE))) %>%
      max()
    search_selection(p.buf$output[[.]]$stdout, basename(course.dir)) %>%
      enter_process(breakline = TRUE)
    . <- search_output(function(.) any(grepl(name, ., fixed = TRUE))) %>%
      max()
    search_selection(p.buf$output[[.]]$stdout, name) %>%
      enter_process(breakline = TRUE)
    src <- system.file(file.path(course.dir, name, "lesson.yaml"), package = "swirl") %>%
      yaml::yaml.load_file()
    ans <- yaml::yaml.load_file(answer.yaml)
    wait_until(function(.) any(grepl("Your status has beed updated to tracking server", ., fixed = TRUE)), is.stdout = FALSE, check.last = TRUE)
    for(i in 2:length(src)) {
      if (src[[i]]$Class == "text") {
        enter_process("\n")
      } else if (src[[i]]$Class == "cmd_question") {
        wait_until(function(.) any(grepl("> ", ., fixed = TRUE)), check.last = TRUE)
        if (!is.null(src[[i]]$CorrectAnswer)) {
          enter_process(src[[i]]$CorrectAnswer, TRUE)
        } else {
          enter_process(ans[[i]]$CorrectAnswer, TRUE)
        }
      } else if (src[[i]]$Class == "script") {
        wait_until(function(.) any(grepl("> ", ., fixed = TRUE)), check.last = TRUE)
        script_temp_path <- get_character("swirl:::.get_e()$script_temp_path")
        correct_script_temp_path <- get_character("swirl:::.get_e()$correct_script_temp_path")
        if (file.exists(correct_script_temp_path)) {
          file.copy(from = correct_script_temp_path, to = script_temp_path, overwrite = TRUE)
        } else {
          cat(ans[[i]]$CorrectAnswer, sep = "\n", file = script_temp_path, append = FALSE)
        }
        #      enter_process("cat(sprintf('output:%s:', swirl:::.get_e()$script_temp_path))\n")
        enter_process("submit()\n")
      } else if (src[[i]]$Class == "mult_question") {
        wait_until(function(.) any(grepl("Selection:", ., fixed = TRUE)), check.last = TRUE)
        ans <- src[[i]]$CorrectAnswer %>% as.character()
        . <- search_output(function(.) any(grepl("Selection:", ., fixed = TRUE))) %>%
          max()
        . <- search_selection(p.buf$output[[.]]$stdout, ans)
        enter_process(., breakline = TRUE)
      } else {
        browser()
        stop()
      }
    }
  }

  # Execusion
  tryCatch({
    wait_until(function(.) any(grepl("> ", ., fixed = TRUE)))
    enter_swirl()
    enter_course(lesson.name)
  }, finally = {
    subprocess::process_terminate(p)
  })
}
