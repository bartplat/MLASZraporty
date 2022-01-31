#' @title Wykresy w raportach z 1. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca poziomy, skumulowany, wykres słupkowy na
#' potrzeby raportu. Jako argument zawierający zbiór danych należy podać ramkę
#' danych będącą wynikiem działania funkcji \code{tab_wykres_ad1()} lub
#' inną, analogiczną.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return
#' @importFrom ggplot2 ggplot geom_bar scale_fill_brewer scale_y_continuous
#' labs geom_text coord_flip theme guides ylab xlab
#' @importFrom dplyr .data
wykres_poziomy_1_ad1 = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))

  ggplot(x, aes(x = .data$typ,
                y = .data$value,
                fill = .data$name)) +
    geom_bar(width = 0.75, stat = "identity", colour = "white") +
    scale_fill_brewer(palette = "RdPu", guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent) +
    labs(fill = NULL,
         title = tytul) +
    geom_text(aes(label = .data$lab),
              position = position_stack(vjust = 0.5), size = 3,
              colour = "#000000") +
    ylab(NULL) +
    xlab(NULL) +
    coord_flip() +
    theme(legend.position = "bottom",
          legend.justification = "left",
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    guides(fill = guide_legend(nrow = 6, byrow = TRUE, reverse = TRUE)) %>%
    return()
}
#' @title Wykresy w raportach z 1. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca wykres warstwowy na potrzeby raportu. Jako
#' argument zawierający zbiór danych należy podać ramkę danych będącą wynikiem
#' działania funkcji \code{tab_wykres_wave_ad1()} lub inną, analogiczną.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return
#' @importFrom ggplot2 ggplot geom_area scale_fill_brewer theme guides ylab xlab
#' labs
#' @importFrom dplyr .data
wave_chart_2_ad1 = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))

  ggplot(x, aes(x = .data$month, y = .data$value, fill = .data$name)) +
    geom_area(position = position_stack(reverse = TRUE), stat = "identity") +
    theme(legend.position = "bottom",
          legend.justification = "left",
          legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          # plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    scale_fill_brewer(palette = "RdPu", guide = guide_legend(reverse = TRUE)) +
    guides(fill = guide_legend(nrow = 6, byrow = TRUE, reverse = TRUE), title = NULL) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Odsetek absolwentów szkoły") +
    xlab("Miesiąc") +
    labs(title = tytul) %>%
    return()
}
#' @title Wykresy w raportach z 1. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca poziomy wykres na potrzeby raportu. Jako
#' argument zawierający zbiór danych należy podać ramkę danych będącą wynikiem
#' działania funkcji \code{tab_wykres_ad1()} lub inną, analogiczną.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return
#' @importFrom ggplot2 ggplot geom_bar scale_fill_brewer scale_y_continuous
#' labs geom_text coord_flip theme guides ylab xlab
#' @importFrom dplyr .data
wykres_poziomy_3_ad1 = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))

  ggplot(x, aes(x = .data$name, y = .data$value)) +
    geom_bar(width = 0.9, stat = "identity", colour = "white", fill = "#f768a1", position = position_dodge(width = 0.9)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_discrete(limits = rev) +
    labs(fill = NULL,
         title = "Odsetek absolwentów Państwa szkoły, którzy w grudniu 2020 roku...") +
    geom_text(aes(label = x$lab), position = position_stack(vjust = 0.5), size = 3, hjust = ifelse(round(x$value * 100) < 6, -1, 0.5)) +
    ylab(NULL) +
    xlab(NULL) +
    coord_flip() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    labs(title = tytul) %>%
    return()
}
#' @title Wykresy w raportach z 1. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca wykres typu boxplot na potrzeby raportu. Wykres
#' pokazuje odsetki.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return
#' @importFrom ggplot2 ggplot geom_errorbar geom_boxplot theme xlab
#' @importFrom dplyr .data
boxplot_proc_4_ad1 = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))

  ggplot(x, aes(x = .data$typ, fill = .data$typ)) +
    geom_errorbar(aes(ymin = .data$q5, ymax = .data$q95), width = 0.5) +
    geom_boxplot(aes(ymin = .data$q5,
                     lower = .data$q25,
                     middle = .data$med,
                     upper = .data$q75,
                     ymax = .data$q95),
                 stat = "identity",
                 width = 0.8) +
    scale_fill_brewer(palette = "RdPu", direction = -1) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    scale_y_continuous(labels = scales::percent) +
    xlab(NULL) +
    labs(title = tytul) %>%
    return()
}
#' @title Wykresy w raportach z 1. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca wykres typu boxplot na potrzeby raportu. Wykres
#' pokazuje wartości liczbowe (zarobki).
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return
#' @importFrom ggplot2 ggplot geom_errorbar geom_boxplot theme xlab
#' @importFrom dplyr .data
boxplot_zl_4_ad1 = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))

  ggplot(x, aes(x = .data$typ, fill = .data$typ)) +
    geom_errorbar(aes(ymin = .data$q5, ymax = .data$q95), width = 0.5) +
    geom_boxplot(aes(ymin = .data$q5,
                     lower = .data$q25,
                     middle = .data$med,
                     upper = .data$q75,
                     ymax = .data$q95),
                 stat = "identity",
                 width = 0.8) +
    scale_fill_brewer(palette = "RdPu", direction = -1) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    xlab(NULL) +
    labs(title = tytul) %>%
    return()
}
#' @title Wykresy w raportach z 1. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca wykres słupkowy, pionowy, skumulowany na
#' potrzeby raportu.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return
#' @importFrom ggplot2 ggplot geom_bar scale_y_continuous labs geom_text ylab
#' xlab theme
#' @importFrom dplyr .data
wyk_pion_skum_5_ad1 = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))

  ggplot(x, aes(x = .data$typ, y = .data$value, fill = .data$name)) +
    geom_bar(width = 0.75, stat = "identity", colour = "white") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "RdPu") +
    labs(fill = NULL,
         title = tytul) +
    geom_text(aes(label = lab), position = position_stack(vjust = 0.5), size = 3,
              colour = "#ffffff") +
    ylab(NULL) +
    xlab(NULL) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "white", size = 1.5),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    guides(fill = guide_legend(nrow = 7, byrow = TRUE)) %>%
    return()
}
#' @title Wykresy w raportach z 1. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca wykres słupkowy, pionowy, skumulowany na
#' potrzeby raportu.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @param os_x zmienna zawierająca etykiety osi x - miesiące lub przedziały
#' @param tytul_x tytuł osi x
#' @export
#' @return
#' @importFrom ggplot2 ggplot geom_bar scale_y_continuous labs geom_text ylab
#' xlab theme
#' @importFrom dplyr .data
wyk_facet_6_ad1 = function(x, tytul, os_x, tytul_x) {
  stopifnot(is_tibble(x))

  os_x = ensym(os_x)

  ggplot(x, aes(x = .data[[os_x]], y = .data$value)) +
    geom_bar(aes(fill = .data$typ), stat = "identity") +
    xlab(tytul_x) +
    facet_grid(cols = vars(.data$typ)) +
    ylab(NULL) +
    ylim(c(0, 1)) +
    scale_fill_brewer(palette = "RdPu", direction = -1) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", colour = "lightgrey"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11),
          panel.grid.major.y = element_line(colour = "lightgrey"),
          panel.grid.minor.y = element_line(colour = "lightgrey")) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = tytul) %>%
    return()
}
