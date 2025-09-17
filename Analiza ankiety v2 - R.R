#zapis wyników do pliku tekstowego
sink("wyniki_analizy.txt", split = TRUE)
#ścieżka do danych wejściowych
data_path <- "C:/Users/FreeaksPC/Downloads/Ankieta badawcza.xlsx"

#instalacja i ładowanie potrzebnych pakietów
need <- c("readxl","tidyverse","janitor","stringr","gtsummary","broom","sandwich","lmtest","margins","pscl","car","ResourceSelection", "cluster","factoextra")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(readxl)
library(tidyverse)
library(janitor)
library(stringr)
library(gtsummary)
library(tibble)
library(purrr)

#wczytanie danych i podstawowe czyszczenie
df <- readxl::read_excel(data_path) |>
  janitor::clean_names()


#funkcje pomocnicze
#normalizacja tekstu (małe litery, bez NA)
txt <- function(x) tolower(trimws(ifelse(is.na(x), "", as.character(x))))

#kodowanie odpowiedzi TAK/NIE
yn <- function(x){
  x0 <- txt(x)
  dplyr::case_when(
    str_detect(x0, "^tak") ~ "tak",
    str_detect(x0, "^nie") ~ "nie",
    TRUE ~ NA_character_
  )
}
#sprawdza, czy tekst zawiera daną opcję (np. z pytań wielokrotnego wyboru)
has_option <- function(x, pattern){
  ifelse(is.na(x), NA, grepl(pattern, x, fixed = TRUE))
}
#testy krzyżowe (Chi2 lub Fisher)
cross_test <- function(x, y){
  tab <- table(x, y)
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(list(table = tab, method = "brak testu (mniej niż 2 poziomy)", result = NA))
  }
  if (any(tab < 5)) {
    res <- stats::fisher.test(tab)
    list(table = tab, method = "Fisher exact", result = res)
  } else {
    res <- stats::chisq.test(tab, correct = TRUE)
    list(table = tab, method = "Chi-square (Yates)", result = res)
  }
}


#wypisywanie wyników testów
print_ct <- function(ct, header=""){
  if (nzchar(header)) cat("\n=== ", header, " ===\n", sep = "")
  print(ct$table)
  cat("\nMetoda:", ct$method, "\n")
  if (!is.na(ct$result)[1]) print(ct$result)
}

#proporcje z 95% CI w podgrupach
group_prop_ci <- function(group, outcome, label = "[proporcje + 95% CI]") {
  # outcome może być: factor("nie","tak") / "nie"/"tak" / TRUE/FALSE
  yes <- ifelse(is.na(outcome), NA,
                ifelse(as.character(outcome) %in% c("tak","TRUE","True","true") | outcome == TRUE, 1, 0))
  g <- as.factor(group)
  lv <- levels(g)
  cat("\n", label, "\n", sep = "")
  for (L in lv) {
    sel <- g == L & !is.na(yes)
    n <- sum(sel)
    s <- sum(yes[sel] == 1)
    if (n > 0) {
      pt <- stats::prop.test(s, n, correct = TRUE)
      p_hat <- 100 * s / n
      ci <- 100 * pt$conf.int
      cat(sprintf(" - %s: %.1f%% (n=%d; 95%% CI: %.1f–%.1f%%)\n", L, p_hat, n, ci[1], ci[2]))
    } else {
      cat(sprintf(" - %s: brak danych\n", L))
    }
  }
}


#funkcja do estymacji modeli logistycznych z opcją błędów odpornych
run_logit <- function(formula, data, label="[LOGIT]", robust=TRUE){
  cat("\n", strrep("=", 60), "\n", label, "\n", strrep("=", 60), "\n", sep="")
  
  # 1) Zbuduj ramkę modelową
  mf <- model.frame(formula, data = data, na.action = na.omit)
  
  # 2) Przekonwertuj wynik (LHS) do 0/1 z "tak"=1 zawsze
  y <- model.response(mf)
  if (is.factor(y)) {
    y_bin <- as.integer(y == "tak")
  } else if (is.logical(y)) {
    y_bin <- as.integer(y)
  } else {
    y_bin <- y
  }
  mm <- model.matrix(attr(mf, "terms"), data = mf)
  
  # 3) Dopasuj logistykę na (y_bin, mm[,-1])
  fit <- glm(y_bin ~ mm[,-1], family = binomial(link="logit"))
  cat("\nN obserwacji (po na.omit):", nobs(fit), "\n")
  
  # Pseudo-R2
  pr2 <- tryCatch(pscl::pR2(fit), error=function(e) NULL)
  if(!is.null(pr2)){
    cat(sprintf("Pseudo-R2 (McFadden): %.3f | (Nagelkerke): %.3f\n",
                as.numeric(pr2["McFadden"]), as.numeric(pr2["Nagelkerke"])))
  }
  
  # VIF (na oryginalnym modelu, gdy się da)
  cat("\nVIF (kolinearność):\n")
  v <- tryCatch(car::vif(glm(formula, data=data, family=binomial(), na.action=na.omit)),
                error=function(e) "VIF niedostępny (np. separacja / brak zmienności)")
  print(v)
  
  # Hosmer–Lemeshow
  hl <- tryCatch(ResourceSelection::hoslem.test(y_bin, fitted(fit), g=10), error=function(e) NULL)
  if(!is.null(hl)){
    cat(sprintf("\nHosmer–Lemeshow: X2=%.3f, df=%d, p=%.4f\n",
                hl$statistic, hl$parameter, hl$p.value))
  }
  
  # Współczynniki z błędami odpornymi
  if(robust){
    V <- sandwich::vcovHC(fit, type="HC0")
    ct <- lmtest::coeftest(fit, vcov.=V)
    est <- ct[,1]; se <- ct[,2]; z <- qnorm(0.975)
    out <- data.frame(
      term = rownames(ct),
      OR   = exp(est),
      CI_L = exp(est - z*se),
      CI_H = exp(est + z*se),
      z    = ct[,3],
      p    = ct[,4],
      row.names = NULL
    )
    cat("\nWspółczynniki (OR, 95% CI, p; błędy odporne):\n")
    print(out, row.names=FALSE, digits=4)
  } else {
    cat("\nWspółczynniki (broom, klasyczne błędy):\n")
    print(broom::tidy(fit, conf.int=TRUE, exponentiate=TRUE))
  }
  
  # Efekty marginalne (na uproszczonym modelu — może się nie udać przy separacji)
  cat("\nEfekty marginalne (AME):\n")
  am <- tryCatch(margins::margins(fit), error=function(e) NULL)
  if(!is.null(am)) print(summary(am)) else cat("nieobliczalne\n")
  
  invisible(fit)
}



#przygotowanie zmiennych analitycznych
# Nazwy kolumn w Twoim pliku (po clean_names):
# plec, wiek, wyksztalcenie, miejsce_zamieszkania,
# jak_czesto_spozywasz_alkohol,
# czy_w_ciagu_ostatnich_2_lat_twoje_nawyki_alkoholowe_sie_zmienily,
# jakie_czynniki_mialy_najwiekszy_wplyw_na_zmiane_twoich_nawykow_mozna_wybrac_kilka,
# czy_pandemia_covid_19_wplynela_na_twoje_zwyczaje_zwiazane_z_piciem_alkoholu,
# jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka,
# czy_probowales_as_alkoholu_ekologicznego_bio,
# czy_kupujesz_napoje_bezalkoholowe_np_piwo_0_percent_bezalkoholowe_wino_gin,
# czy_w_ciagu_ostatnich_2_lat_zauwazyles_as_wzrost_cen_alkoholu,
# jak_wzrost_cen_alkoholu_wplynal_na_twoje_zachowanie_mozna_wybrac_kilka,
# czy_zmieniles_as_miejsce_zakupu_alkoholu_w_wyniku_wzrostu_cen,
# czy_w_wyniku_wzrostu_cen_siegnales_as_po_alkohole_z_tzw_szarej_strefy_np_bez_akcyzy,
# czy_uwazasz_ze_kampanie_rzadowe_spoleczne_maja_wplyw_na_ograniczenie_spozycia_alkoholu,
# czy_zauwazyles_as_ograniczenia_reklamy_i_sprzedazy_alkoholu,
# czy_planujesz_ograniczyc_spozycie_alkoholu_w_przyszlosci,
# co_skloniloby_cie_do_calkowitej_rezygnacji_z_alkoholu_mozna_wybrac_kilka

# Główne binaria i grupy
df <- df |>
  mutate(
    # płeć jako factor
    plec_bin = factor(plec),
    # grupy wieku 18–34 vs 35+
    wiek_18_34 = if_else(wiek %in% c("18-24","25-34"), "18–34", "35+"),
    duze_miasto = if_else(miejsce_zamieszkania == "Miasto powyżej 200 tys.", "duze","mniejsze"),
    wykszt_niskie = if_else(wyksztalcenie %in% c("Podstawowe","Średnie"), "niskie","wysokie"),
    # tworzenie wskaźników dot. częstotliwości, napojów 0%, eko, wzrostu cen itd.
    # częste spożycie: codziennie / kilka razy w tygodniu / raz w tygodniu
    czeste = if_else(str_detect(txt(jak_czesto_spozywasz_alkohol),
                                "codzien|kilka razy w tyg|raz w tyg"),
                     "tak","nie"),
    # kupuje bezalkoholowe: "tak - regularnie" lub "czasami"
    kupuje_0 = case_when(
      str_detect(txt(czy_kupujesz_napoje_bezalkoholowe_np_piwo_0_percent_bezalkoholowe_wino_gin),
                 "tak|czasami") ~ "tak",
      txt(czy_kupujesz_napoje_bezalkoholowe_np_piwo_0_percent_bezalkoholowe_wino_gin) == "" ~ NA_character_,
      TRUE ~ "nie"
    ),
    # eko/bio – "tak - kupuję" / podobne
    kupuje_eko = case_when(
      str_detect(txt(czy_probowales_as_alkoholu_ekologicznego_bio), "tak|kupuj") ~ "tak",
      txt(czy_probowales_as_alkoholu_ekologicznego_bio) == "" ~ NA_character_,
      TRUE ~ "nie"
    ),
    # wzrost cen zauważony
    wzrost_cen = yn(czy_w_ciagu_ostatnich_2_lat_zauwazyles_as_wzrost_cen_alkoholu),
    # zmiana miejsca na tańsze – tak/nie
    zmiana_miejsca_tansze = yn(czy_zmieniles_as_miejsce_zakupu_alkoholu_w_wyniku_wzrostu_cen),
    # szara strefa – tak/nie
    szara_strefa = yn(czy_w_wyniku_wzrostu_cen_siegnales_as_po_alkohole_z_tzw_szarej_strefy_np_bez_akcyzy),
    # zauważone ograniczenia reklam/sprzedaży
    zauw_ograniczenia = yn(czy_zauwazyles_as_ograniczenia_reklamy_i_sprzedazy_alkoholu),
    # plan ograniczenia
    planuje_ograniczyc = yn(czy_planujesz_ograniczyc_spozycie_alkoholu_w_przyszlosci),
    # zmiana nawyków 2 lata
    zmiana_nawykow_2l = yn(czy_w_ciagu_ostatnich_2_lat_twoje_nawyki_alkoholowe_sie_zmienily),
    # wpływ pandemii
    covid_wplyw = yn(czy_pandemia_covid_19_wplynela_na_twoje_zwyczaje_zwiazane_z_piciem_alkoholu)
  )

# Wielokrotne wybory → flagi
df <- df |>
  mutate(
    #flagi dla pytań wielokrotnego wyboru (rodzaje alkoholu, reakcje na ceny, powody zmiany)
    rtd   = has_option(jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, "Gotowe koktajle (RTD)"),
    mocne = has_option(jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, "Wódka") |
      has_option(jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, "Whisky/Whiskey") |
      has_option(jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, "Gin"),
    nisko = has_option(jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, "Piwo") |
      has_option(jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, "Wino") |
      has_option(jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, "Gotowe koktajle (RTD)"),
    # WPŁYW CEN
    rzadziej        = has_option(jak_wzrost_cen_alkoholu_wplynal_na_twoje_zachowanie_mozna_wybrac_kilka, "Rzadziej kupuję alkohol"),
    tansze_marki    = has_option(jak_wzrost_cen_alkoholu_wplynal_na_twoje_zachowanie_mozna_wybrac_kilka, "Wybieram tańsze marki"),
    mniejsze_ilosci = has_option(jak_wzrost_cen_alkoholu_wplynal_na_twoje_zachowanie_mozna_wybrac_kilka, "Kupuję mniejsze ilości"),
    zmiana_miejsca  = has_option(jak_wzrost_cen_alkoholu_wplynal_na_twoje_zachowanie_mozna_wybrac_kilka, "Zmieniam miejsce zakupu na tańsze"),
    brak_wplywu_cen = has_option(jak_wzrost_cen_alkoholu_wplynal_na_twoje_zachowanie_mozna_wybrac_kilka, "Nie zauważyłem/am wpływu"),
    # POWODY ZMIANY NAWYKÓW
    powod_ceny        = has_option(jakie_czynniki_mialy_najwiekszy_wplyw_na_zmiane_twoich_nawykow_mozna_wybrac_kilka, "Wzrost cen alkoholu"),
    powod_zdrowie     = has_option(jakie_czynniki_mialy_najwiekszy_wplyw_na_zmiane_twoich_nawykow_mozna_wybrac_kilka, "Wzrost świadomości zdrowotnej"),
    powod_tryb        = has_option(jakie_czynniki_mialy_najwiekszy_wplyw_na_zmiane_twoich_nawykow_mozna_wybrac_kilka, "Zmiana trybu życia"),
    powod_srodowisko  = has_option(jakie_czynniki_mialy_najwiekszy_wplyw_na_zmiane_twoich_nawykow_mozna_wybrac_kilka, "Zmiana środowiska (np. praca, relacje)"),
    # Kampanie (opinia Likert) → binarnie: czy w ogóle "tak"
    kampanie_tak = case_when(
      str_detect(txt(czy_uwazasz_ze_kampanie_rzadowe_spoleczne_maja_wplyw_na_ograniczenie_spozycia_alkoholu),
                 "tak|raczej tak|zdecydowanie tak") ~ "tak",
      txt(czy_uwazasz_ze_kampanie_rzadowe_spoleczne_maja_wplyw_na_ograniczenie_spozycia_alkoholu) == "" ~ NA_character_,
      TRUE ~ "nie"
    ),
    # Co skłoniłoby do rezygnacji – flaga dla „Kampania edukacyjna”
    kampania_eduk = has_option(co_skloniloby_cie_do_calkowitej_rezygnacji_z_alkoholu_mozna_wybrac_kilka, "Kampania edukacyjna")
  ) |>
  mutate(
    # Złożone pojęcie do H9: "częste mocne"
    czeste_mocne = if_else(czeste=="tak" & mocne, "tak","nie")
  )%>%
  mutate(across(
    c(kupuje_0, kupuje_eko, wzrost_cen, zmiana_miejsca_tansze,
      szara_strefa, zauw_ograniczenia, planuje_ograniczyc,
      zmiana_nawykow_2l, covid_wplyw, czeste_mocne),
    ~ factor(., levels = c("nie","tak"))
  ))

#testowanie hipotez H1–H12
# H1: Kobiety częściej nisko/bezalkoholowe?
print_ct(cross_test(df$plec_bin, factor(if_else(df$kupuje_0=="tak","tak","nie"))), "H1: płeć × napoje 0%")
# --- H1: przedziały ufności dla różnicy proporcji + OR ---
tab_h1 <- table(df$plec_bin, factor(if_else(df$kupuje_0=="tak","tak","nie")))
if (all(dim(tab_h1) == c(2,2))) {
  cat("\n[H1] prop.test (różnica proporcji + 95% CI):\n")
  print(stats::prop.test(tab_h1))
  
  cat("\n[H1] fisher.test (OR + 95% CI):\n")
  print(stats::fisher.test(tab_h1))
}
# H2: 18–34 częściej RTD?
print_ct(cross_test(df$wiek_18_34, factor(if_else(df$rtd, "tak","nie"))), "H2: wiek (18–34) × RTD")

# H3: Duże miasto × eko?
print_ct(cross_test(df$duze_miasto, factor(if_else(df$kupuje_eko=="tak","tak","nie"))), "H3: duże miasto × eko")

# H4: Wzrost cen → rzadziej?
print_ct(cross_test(df$wzrost_cen, factor(if_else(df$rzadziej, "tak","nie"))), "H4: wzrost cen × rzadziej")
group_prop_ci(df$wzrost_cen, factor(if_else(df$rzadziej, "tak","nie")),
              label = "[H4] Udział 'rzadziej kupuję' w grupach (wzrost cen: nie/tak)")

# H5: Wśród zmieniających nawyki: najczęstszy powód = ceny?
cat("\n=== H5: Powody zmiany (tylko ci, którzy zmienili) ===\n")
sub <- df |> filter(zmiana_nawykow_2l == "tak")
counts_h5 <- tibble(
  powod = c("Wzrost cen alkoholu","Wzrost świadomości zdrowotnej","Zmiana trybu życia","Zmiana środowiska"),
  n = c(sum(sub$powod_ceny, na.rm=TRUE),
        sum(sub$powod_zdrowie, na.rm=TRUE),
        sum(sub$powod_tryb, na.rm=TRUE),
        sum(sub$powod_srodowisko, na.rm=TRUE))
) |> arrange(desc(n))
print(counts_h5)

# H6: COVID → zmiana nawyków?
print_ct(cross_test(df$covid_wplyw, df$zmiana_nawykow_2l), "H6: COVID × zmiana nawyków")
group_prop_ci(df$covid_wplyw, df$zmiana_nawykow_2l,
              label = "[H6] Udział 'zmiana nawyków' w grupach COVID (nie/tak)")

# H7: Wykształcenie / wieś → zmiana miejsca na tańsze?
print_ct(cross_test(df$wykszt_niskie, df$zmiana_miejsca_tansze), "H7a: wykształcenie × zmiana miejsca (tańsze)")
wies <- if_else(df$miejsce_zamieszkania == "Wieś", "wies", "nie_wies")
print_ct(cross_test(wies, df$zmiana_miejsca_tansze), "H7b: wieś × zmiana miejsca (tańsze)")

# H8: Wzrost cen → szara strefa? (porównanie grup)
cat("\n=== H8: Szara strefa – porównanie wg wzrostu cen ===\n")
tab_h8_2x2 <- table(df$wzrost_cen, df$szara_strefa)
print(tab_h8_2x2)

# test niezależności (Fisher/χ²)
print_ct(cross_test(df$wzrost_cen, df$szara_strefa), "H8: wzrost cen × szara strefa")

# proporcje + 95% CI i p-value różnicy
if (all(dim(tab_h8_2x2) == c(2,2))) {
  cat("\n[H8] prop.test (różnica proporcji + 95% CI):\n")
  print(stats::prop.test(tab_h8_2x2))
  cat("\n[H8] fisher.test (OR + 95% CI):\n")
  print(stats::fisher.test(tab_h8_2x2))
}


# H9: Zauważone ograniczenia × częste mocne?
print_ct(cross_test(df$zauw_ograniczenia, df$czeste_mocne), "H9: ograniczenia reklamy/sprzedaży × częste mocne")

# H10: Plan ograniczenia × (1) wpływ kampanii, (2) kampania edukacyjna
print_ct(cross_test(df$planuje_ograniczyc, df$kampanie_tak), "H10a: plan ograniczenia × 'kampanie mają wpływ'")
group_prop_ci(df$planuje_ograniczyc, df$kampanie_tak,
              label = "[H10a] Udział 'kampanie mają wpływ' w grupach planu ograniczenia (nie/tak)")

print_ct(cross_test(df$planuje_ograniczyc, factor(if_else(df$kampania_eduk, "tak","nie"))), "H10b: plan ograniczenia × 'kampania edukacyjna'")

# H11: Wzrost cen → tańsze marki?
print_ct(cross_test(df$wzrost_cen, factor(if_else(df$tansze_marki, "tak","nie"))), "H11: wzrost cen × tańsze marki")
group_prop_ci(df$wzrost_cen, factor(if_else(df$tansze_marki, "tak","nie")),
              label = "[H11] Udział 'wybieram tańsze marki' w grupach (wzrost cen: nie/tak)")
tab <- table(df$wzrost_cen, factor(if_else(df$tansze_marki, "tak","nie")))
if (all(dim(tab) == c(2,2))) {
  a <- tab[2,2]; b <- tab[2,1]; c <- tab[1,2]; d <- tab[1,1]
  or_ha <- ((a + 0.5)*(d + 0.5))/((b + 0.5)*(c + 0.5))
  cat(sprintf("[H11] OR (Haldane–Anscombe) = %.2f (korekcja 0.5)\n", or_ha))
}

# H12: Bezalkoholowe × (1) zmiana trybu życia (powód), (2) plan ograniczenia
print_ct(cross_test(df$kupuje_0, factor(if_else(df$powod_tryb, "tak","nie"))), "H12a: napoje 0% × 'zmiana trybu życia'")
print_ct(cross_test(df$kupuje_0, df$planuje_ograniczyc), "H12b: napoje 0% × plan ograniczenia")

#tabele opisowe
cat("\n=== Opisowe: demografia i częstotliwość ===\n")
print(
  df |>
    select(plec_bin, wiek, wyksztalcenie, miejsce_zamieszkania, jak_czesto_spozywasz_alkohol) |>
    gtsummary::tbl_summary()
)

cat("\n=== Rozkład rodzajów alkoholu (udział %) ===\n")
#udział poszczególnych kategorii alkoholu
rodzaje <- c("Piwo","Wino","Wódka","Whisky/Whiskey","Gin","Gotowe koktajle (RTD)","Napoje bezalkoholowe (np. piwo 0%, wino 0%)")
dist_df <- purrr::map_dfr(rodzaje, ~ tibble(
  kategoria = .x,
  udzial = mean(has_option(df$jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, .x), na.rm=TRUE)
)) |>
  mutate(udzial = round(100*udzial,1)) |>
  arrange(desc(udzial))
print(dist_df)



cat("\n\n MODELE LOGISTYCZNE DLA WYBRANYCH HIPOTEZ (skorygowane) \n")

# (H4) Czy wzrost cen zwiększa szansę kupowania rzadziej? + kontrolne
m_H4 <- run_logit(
  rzadziej ~ wzrost_cen + plec_bin + wiek_18_34 + wykszt_niskie + duze_miasto,
  df, label="[H4] rzadziej ~ wzrost_cen + kontrole"
)

# (H11) Czy wzrost cen zwiększa wybór tańszych marek? + kontrolne
m_H11 <- run_logit(
  tansze_marki ~ wzrost_cen + plec_bin + wiek_18_34 + wykszt_niskie + duze_miasto,
  df, label="[H11] tansze_marki ~ wzrost_cen + kontrole"
)

# (H8) Czy wzrost cen wiąże się z sięganiem po szarą strefę? + kontrolne
m_H8 <- run_logit(
  szara_strefa ~ wzrost_cen + plec_bin + wiek_18_34 + wykszt_niskie + duze_miasto,
  df, label="[H8] szara_strefa ~ wzrost_cen + kontrole"
)

# (H9) Czy zauważone ograniczenia reklamy/sprzedaży wiążą się z częstym piciem mocnych? + kontrolne
m_H9 <- run_logit(
  czeste_mocne ~ zauw_ograniczenia + plec_bin + wiek_18_34 + wykszt_niskie + duze_miasto,
  df, label="[H9] czeste_mocne ~ zauw_ograniczenia + kontrole"
)

# (H10) Co wpływa na plan ograniczenia spożycia? (model wieloczynnikowy)
m_H10 <- run_logit(
  planuje_ograniczyc ~ wzrost_cen + kampanie_tak + covid_wplyw + kupuje_0 +
    plec_bin + wiek_18_34 + wykszt_niskie + duze_miasto,
  df, label="[H10] planuje_ograniczyc ~ czynniki + kontrole"
)

# (H3) Czy kupowanie eko jest wyższe w dużych miastach? + profil socjo
m_H3 <- run_logit(
  kupuje_eko ~ duze_miasto + plec_bin + wiek_18_34 + wykszt_niskie,
  df, label="[H3] kupuje_eko ~ duze_miasto + kontrole"
)

# (uzupełniający) Kto wybiera napoje 0%? (styl konsumpcji + socjo)
m_0 <- run_logit(
  kupuje_0 ~ czeste + mocne + rtd + nisko + plec_bin + wiek_18_34 + wykszt_niskie + duze_miasto,
  df, label="[DOD] kupuje_0 ~ styl + socjo"
)



cat("\n\n KLASTROWANIE: STYL KONSUMPCJI \n")
# Zmienne stylu (0/1)
styl_df <- df |>
  transmute(
    rtd   = as.integer(rtd),
    mocne = as.integer(mocne),
    nisko = as.integer(nisko),
    zero  = as.integer(kupuje_0 == "tak"),
    czeste = as.integer(czeste == "tak")
  ) |>
  tidyr::drop_na()

if(nrow(styl_df) >= 20){
  set.seed(123)  
  diss_styl <- cluster::daisy(styl_df, metric="gower")
  sil <- data.frame(k=2:6, sil=NA_real_)
  for(k in 2:6){
    pamk <- cluster::pam(diss_styl, k=k, diss=TRUE)
    sil[sil$k==k,"sil"] <- pamk$silinfo$avg.width
  }
  print(sil)
  k_best <- sil$k[which.max(sil$sil)]
  cat(sprintf("\nWybrano k=%d (najwyższa średnia sylwetka).\n", k_best))
  cat(sprintf("Śr. szerokość sylwetki (k=%d): %.3f\n", k_best, max(sil$sil, na.rm=TRUE)))
  
  pam_styl <- cluster::pam(diss_styl, k=k_best, diss=TRUE)
  cl_styl <- factor(pam_styl$clustering)
  cat("\nWielkości klastrów (styl):\n"); print(table(cl_styl))
  
  # Profilowanie: odsetki cech w klastrach
  prof_styl <- styl_df |>
    mutate(cluster = cl_styl) |>
    group_by(cluster) |>
    summarise(across(everything(), ~ round(mean(.)*100,1)), .groups="drop")
  cat("\nProfile klastrów (odsetki % cech):\n")
  print(prof_styl)
  
  # (opcjonalnie) 2D do szybkiej wizualizacji
  # fviz_cluster(pam_styl, geom="point")  # wymaga factoextra
} else {
  cat("Za mało obserwacji po usunięciu NA do klastrów stylu.\n")
}

cat("\n\n KLASTROWANIE: REAKCJA NA CENY \n")
# Zmienne reakcji cenowej (0/1)
reakcje_df <- df |>
  transmute(
    rzadziej        = as.integer(rzadziej),
    tansze_marki    = as.integer(tansze_marki),
    mniejsze_ilosci = as.integer(mniejsze_ilosci),
    zmiana_miejsca  = as.integer(zmiana_miejsca)
  ) |>
  tidyr::drop_na()

if(nrow(reakcje_df) >= 20){
  set.seed(123)  
  diss_reakcje <- cluster::daisy(reakcje_df, metric="gower")
  sil2 <- data.frame(k=2:6, sil=NA_real_)
  for(k in 2:6){
    pamk <- cluster::pam(diss_reakcje, k=k, diss=TRUE)
    sil2[sil2$k==k,"sil"] <- pamk$silinfo$avg.width
  }
  print(sil2)
  k_best2 <- sil2$k[which.max(sil2$sil)]
  cat(sprintf("\nWybrano k=%d (najwyższa średnia sylwetka).\n", k_best2))
  cat(sprintf("Śr. szerokość sylwetki (k=%d): %.3f\n", k_best2, max(sil2$sil, na.rm=TRUE)))
  pam_re <- cluster::pam(diss_reakcje, k=k_best2, diss=TRUE)
  cl_re <- factor(pam_re$clustering)
  cat("\nWielkości klastrów (reakcje cenowe):\n"); print(table(cl_re))
  
  prof_re <- reakcje_df |>
    mutate(cluster = cl_re) |>
    group_by(cluster) |>
    summarise(across(everything(), ~ round(mean(.)*100,1)), .groups="drop")
  cat("\nProfile klastrów (odsetki % zachowań):\n")
  print(prof_re)
} else {
  cat("Za mało obserwacji po usunięciu NA do klastrów reakcji na ceny.\n")
}


#MODEL DODATKOWY / KTO ZAUWAŻYŁ WZROST CEN?
# 7 opcji odpowiedzi (jednokrotnego wyboru)
# Ustawiono bazę "dyskont"

df <- df %>%
  mutate(
    kanal_7 = case_when(
      str_detect(txt(gdzie_najczesciej_kupujesz_alkohol), "^nie kupuj") ~ "nie_kupuje",
      str_detect(txt(gdzie_najczesciej_kupujesz_alkohol), "dyskont") ~ "dyskont",
      str_detect(txt(gdzie_najczesciej_kupujesz_alkohol), "supermarket|carrefour|auchan|kaufland|hipermarket") ~ "supermarket",
      str_detect(txt(gdzie_najczesciej_kupujesz_alkohol), "stacja") ~ "stacja",
      str_detect(txt(gdzie_najczesciej_kupujesz_alkohol), "osiedlow") ~ "osiedlowy",
      str_detect(txt(gdzie_najczesciej_kupujesz_alkohol), "internet|online|e-") ~ "internet",
      str_detect(txt(gdzie_najczesciej_kupujesz_alkohol), "monopol") ~ "monopolowy",
      TRUE ~ NA_character_
    ),
    kanal_7 = factor(
      kanal_7,
      levels = c("dyskont","supermarket","osiedlowy","monopolowy","stacja","internet","nie_kupuje")
    )
  )

# Dominujący typ napoju: piwo / wino / mocne (priorytet: mocne > wino > piwo)
df <- df %>%
  mutate(
    piwo_flag = has_option(jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, "Piwo"),
    wino_flag = has_option(jakie_rodzaje_alkoholu_spozywasz_najczesciej_mozna_wybrac_kilka, "Wino"),
    typ_glowny = case_when(
      mocne ~ "mocne",
      wino_flag ~ "wino",
      piwo_flag ~ "piwo",
      TRUE ~ NA_character_
    ),
    typ_glowny = factor(typ_glowny, levels = c("piwo","wino","mocne"))
  )

# szybki podgląd liczebności:
table(df$kanal_7, useNA = "ifany")


mCEN_M3 <- run_logit(
  wzrost_cen ~ plec_bin + wiek_18_34 + wykszt_niskie + duze_miasto +
    czeste + typ_glowny + kanal_7,
  df, label="[CEN-M3] wzrost_cen ~ demografia + styl + kanał zakupu"
)




sink()