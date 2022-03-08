wskaznikiZawPow$UCZ_zawod = unlist(wskaznikiZawPow$UCZ_zawod)
stanFolderu = list.files()

################################################################################
context("Generowanie raportów PDF - szkoły")
################################################################################
wybraneSzkoly = c(3, 4, 5, 11, 12, 14)
test_that("Raporty PDF na podstawie szablonu 'raport_szkoly.RMD'", {
  nazwyRaportowPDF =
    generuj_raporty('raport_szkoly_1rm.Rmd',
                    wskaznikiSzk[wybraneSzkoly, ],
                    wskaznikiSzkGrPor[wybraneSzkoly, ],
                    SZK_kod,
                    parametry = list(typDokumentu = "pdf",
                                     progLiczebnosci = 10,
                                     rocznik = 2017,
                                     wyrownanieTabWykr = "center"))
  expect_equal(paste0(wskaznikiSzk$SZK_kod, ".pdf")[wybraneSzkoly],
               nazwyRaportowPDF)
})

################################################################################
context("Generowanie raportów HTML - szkoły")
################################################################################
test_that("Raporty HTML na podstawie szablonu 'raport_szkoly.RMD'", {
  nazwyRaportowHTML =
    generuj_raporty('raport_szkoly_1rm.Rmd',
                    wskaznikiSzk[wybraneSzkoly, ],
                    wskaznikiSzkGrPor[wybraneSzkoly, ],
                    SZK_kod,
                    parametry = list(typDokumentu = "html",
                                     progLiczebnosci = 10,
                                     rocznik = 2017,
                                     wyrownanieTabWykr = "center"))
  expect_equal(paste0(wskaznikiSzk$SZK_kod, ".html")[wybraneSzkoly],
               nazwyRaportowHTML)
})

################################################################################
context("Generowanie raportów PDF - powiato-zawody")
################################################################################
test_that("Raporty PDF na podstawie szablonu 'raport_powiaty.RMD'", {
  nazwyRaportowPDF =
    generuj_raporty('raport_powiaty_1rm.Rmd',
                    wskaznikiZawPow,
                    wskaznikiZawPowGrPor,
                    ABS_teryt_powiat,
                    parametry = list(typDokumentu = "pdf",
                                     progLiczebnosci = 10,
                                     rocznik = 2017,
                                     wyrownanieTabWykr = "center"))
  expect_equal(paste0(wskaznikiZawPow$ABS_teryt_powiat, ".pdf"),
               nazwyRaportowPDF)
})

################################################################################
context("Generowanie raportów HTML - powiato-zawody")
################################################################################
test_that("Raporty HTML na podstawie szablonu 'raport_powiaty.RMD'", {
  nazwyRaportowHTML =
    generuj_raporty('raport_powiaty_1rm.Rmd',
                    wskaznikiZawPow,
                    wskaznikiZawPowGrPor,
                    ABS_teryt_powiat,
                    parametry = list(typDokumentu = "html",
                                     progLiczebnosci = 10,
                                     rocznik = 2017,
                                     wyrownanieTabWykr = "center"))
  expect_equal(paste0(wskaznikiZawPow$ABS_teryt_powiat, ".html"),
               nazwyRaportowHTML)
})
################################################################################
context("Wywołania błędów wejścia dla generuj_raporty()")
################################################################################
test_that("Nieistniejący szablon raportu", {
  expect_error(generuj_raporty('raport_ktorego_nie_ma.Rmd',
                               wskaznikiSzk,
                               wskaznikiSzkGrPor,
                               SZK_kod),
               "Szablon o podanej nazwie nie jest dostępny")
})
test_that("Niepodane parametry wymagane przez szablon", {
  expect_error(generuj_raporty('raport_szkoly_1rm.Rmd',
                               wskaznikiSzk,
                               wskaznikiSzkGrPor,
                               SZK_kod),
               "Szablon.*wymaga.*parametrów")
})
test_that("Niepodanie danych opisujących grupy porównawcze (gdy szablon ich wymaga)", {
  expect_error(generuj_raporty('raport_szkoly_1rm.Rmd',
                               wskaznikiSzk,
                               parametry = list(typDokumentu = "html",
                                                progLiczebnosci = 10,
                                                rocznik = 2017,
                                                wyrownanieTabWykr = "center")),
               "Szablon.*wymaga przekazania ramki danych.*w grupach porównawczych")
})
test_that("Typ dokumentu inny niż PDF lub HTML", {
  expect_error(generuj_raporty('raport_szkoly_1rm.Rmd',
                               wskaznikiSzk,
                               wskaznikiSzkGrPor,
                               SZK_kod,
                               parametry = list(typDokumentu = "txt",
                                                progLiczebnosci = 10,
                                                rocznik = 2017,
                                                wyrownanieTabWykr = "center")),
               "musi przyjmować wartość 'html' lub 'pdf'")
})
test_that("Polskie znaki w kolumnie opisującej nazwy raportów", {
  wskaznikiSzk$SZK_typ = unlist(wskaznikiSzk$SZK_typ)
  expect_error(generuj_raporty('raport_szkoly_1rm.Rmd',
                               wskaznikiSzk,
                               wskaznikiSzkGrPor,
                               SZK_typ),
               "nie mogą zawierać polskich znaków")
})
test_that("Duplikaty w kolumnie opisującej nazwy raportów", {
  wskaznikiSzk$SZK_kod[1] = wskaznikiSzk$SZK_kod[2]
  expect_error(generuj_raporty('raport_szkoly_1rm.Rmd',
                               wskaznikiSzk,
                               wskaznikiSzkGrPor,
                               SZK_kod),
               "muszą być unikalne")
})

################################################################################

unlink(setdiff(list.files(), stanFolderu), recursive = TRUE)
