
#' make sure these packages are installed
.p <- c("ggplot2", "dplyr", "rvest", "tidyr")
if (any(!.p %in% installed.packages())) {
    install.packages(.p[!.p %in% installed.packages()])
}

#' make url
#'
#' @param x president last name
#' @return URL.
.url <- function(x) {
    if (grepl("trump", x, ignore.case = TRUE)) {
        x <- "president_trump_job_approval-6179"
    } else if (grepl("obama", x, ignore.case = TRUE)) {
        x <- "president_obama_job_approval-1044"
    } else if (grepl("bush", x, ignore.case = TRUE)) {
        x <- "president_bush_job_approval-904"
    }
    paste0("http://www.realclearpolitics.com/epolls/other/",
           x, ".html#polls")
}

#' get approval ratings
#'
#' @param pres President last name.
#' @return Returns data as tbl
get_approval <- function(pres = "trump") {
    require(rvest)
    require(dplyr)
    ## scrape html
    df <- .url(pres) %>%
        read_html() %>%
        html_table() %>%
        .[[length(.)]] %>%
        dplyr::tbl_df()
    ## format names
    names(df) <- tolower(names(df))
    ## create pop variable
    if (grepl("bush", pres, ignore.case = TRUE)) {
        x <- dplyr::data_frame(
            n = rep(NA_real_, nrow(df)),
            pop = rep(NA_character_, nrow(df)))
        df <- dplyr::select(df, -spread)
    } else {
        x <- df$sample %>%
            strsplit(" ") %>%
            lapply(function(x) dplyr::data_frame(n = x[1], pop = x[2]))
        x <- do.call(dplyr::bind_rows, x)
        df <- dplyr::select(df, -sample)
    }
    ## merge columns
    dplyr::bind_cols(df, x)
}

## get trump approval data
trump <- get_approval("trump")
trump$pres <- "Trump"

## obama's
obama <- get_approval("obama")
obama$pres <- "Obama"

## bush's
bush <- get_approval("bush")
bush$pres <- "Bush II"

## collapse trump and obama and filter out NA pop obs
df <- bind_rows(trump, obama) %>%
    filter(pop != "" & !is.na(pop))
## recode and change level order
df$pop <- df %>%
    with(., pop) %>%
    dplyr::recode(A = "Adults",
                  LV = "LVs",
                  RV = "RVs") %>%
    factor(levels = c("Adults", "RVs", "LVs"))

## load ggplot2
require(ggplot2)

## convert to long format and plot disapproval ratings
p1 <- df %>%
    dplyr::select(pres, pop, disapprove) %>%
    tidyr::gather(var, val, -pres, -pop) %>%
    ggplot(aes(x = val, fill = pres, color = pres)) +
    geom_density(alpha = .65) +
    theme_minimal() +
    facet_wrap(~ pop, ncol = 1,
               strip.position = "left") +
    scale_fill_manual(
        values = c(Obama = "#2244dd", Trump = "#dd3333")) +
    scale_color_manual(
        values = c(Obama = "#002277", Trump = "#771111")) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(hjust = .5),
          axis.text.y = element_blank(),
          strip.text = element_text(size = 12),
          text = element_text(family = "Roboto")) +
    xlim(35, 60) +
    labs(x = "Poll est.", y = NULL, title = "Disapprove")

## convert to long format and plot approval ratings
p2 <- df %>%
    dplyr::select(pres, pop, approve) %>%
    tidyr::gather(var, val, -pres, -pop) %>%
    ggplot(aes(x = val, fill = pres, color = pres)) +
    geom_density(alpha = .65) +
    theme_minimal() +
    facet_wrap(~ pop, ncol = 1) +
    scale_fill_manual(
        values = c(Obama = "#2244dd", Trump = "#dd3333")) +
    scale_color_manual(
        values = c(Obama = "#002277", Trump = "#771111")) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(hjust = .5),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          text = element_text(family = "Roboto")) +
    xlim(35, 60) +
    labs(x = "Poll est.", y = NULL, title = "Approve")

## save plot output
png("../approval-pop.png", 6.5, 7, "in", res = 127.5)
gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(.52, .48))
dev.off()

## create text for README
readme <- paste("## Scraping and plotting approval ratings",
                "![approval-pop.png](approval-pop.png)",
                sep = "\n\n")

## save README
cat(readme, file = "../README.md", fill = TRUE)
