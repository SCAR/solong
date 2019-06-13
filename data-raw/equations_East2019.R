refs$East2019 <- bibentry(bibtype = "Article", key = "East2019",
                          author = c(person(c("J", "T"), "Eastman")),
                          year = 2019,
                          title = "An analysis of maximum body size and designation of size categories for notothenioid fishes",
                          journal = "Polar Biology", volume = 42, pages = "1131-1145",
                          doi="10.1007/s00300-019-02502-7")

alleq_East2019 <- function(id) {
    processed_data_file <- "data-raw/East2019_processed.rds"
    if (!file.exists(processed_data_file)) {
        tempeq <- tibble::tribble(
                              ~sp,  ~tl,                                                                                                                                                          ~ref,
                              "Percophis brasiliensis",   74,                                                                                                        "Barretto et al. (2011); Perez Comesaña et al. (2014)",
                              "Bovichtus angustifrons",   30,                                                                                                            "Hardy (1988); Gomon and Last (2008); Bray (2017)",
                              "B. chilensis", 34.2,                                                                                                                                  "Navarro and Pequeño (1979)",
                              "B. diacanthus",   25,                                                                                                                                        "Andrew et al. (1995)",
                              "B. oculus", 12.7,                                                                                                               "Stewart (2015a); A. Stewart pers. com. (2017)",
                              "B. psychrolutes", 21.7,                                                                                                 "Hardy (1988); Stewart (2015a); A. Stewart pers. com. (2018)",
                              "B. variegatus",   28,                                                                                                               "Stewart (2015a); A. Stewart pers. com. (2018)",
                              "B. veneris", 21.7,                                                                                                                                              "Duhamel (1989)",
                              "Cottoperca trigloides",   80,                                                                                                                           "Laptikhovsky and Arkhipkin (2003)",
                              "Halaphritis platycephala",   19,                                                                                                                   "Last et al. (2002); Gomon and Last (2008)",
                              "Pseudaphritis urvillii",   36,                                                                                               "Raadik (2008); Harris et al. (2013); Bray and Thompson (2017)",
                              "Eleginops maclovinus",   80,                                                                                                      "Brickle et al. (2003; 2005a;b); Licandeo et al. (2006)",
                              "Aethotaxis mitopteryx",   52,                                                                              "DeWitt et al. (1990); Kunzmann and Zimmermann (1992); La Mesa et al. (2018a;b)",
                              "Dissostichus eleginoides",  225,                                              "DeWitt et al. (1990); des Clers et al. (1996); Duhamel et al. (2005); Laptikhovsky et al. (2006); Arana (2009)",
                              "D. mawsoni",  210,                                                                                                                 "DeWitt et al. (1990); Hanchet et al. (2015)",
                              "Pleuragramma antarctica",   27,                                                                                                                "DeWitt et al. (1990); Sutton and Horn (2011)",
                              "Gvozdarus svetovidovi", 61.3,                                                                                                                                "Shandikov and Kratkiy (1990)",
                              "Trematomus amphitreta", 30.3,                                                                                                                                      "Cziko and Cheng (2006)",
                              "T. bernacchii",   35,                                                                                                                                        "DeWitt et al. (1990)",
                              "T. borchgrevinki",   28,                                                                                                                                        "DeWitt et al. (1990)",
                              "T. brachysoma",   17,                                                                                                                                        "DeWitt et al. (1990)",
                              "T. eulepidotus", 34.5,                                                                                                                    "DeWitt et al. (1990); Kock et al. (2000)",
                              "T. hansoni", 45.5,                                                                                              "DeWitt et al. (1990); Kock et al. (2000); Vacchi et al. (2000)",
                              "T. lepidorhinus",   31,                                                                                                                                        "DeWitt et al. (1990)",
                              "T. loennbergii",   33,                                                                                           "DeWitt et al. (1990); La Mesa et al. (1997); Vacchi et al. (2000)",
                              "T. newnesi", 24.9,                                                                                       "DeWitt et al. (1990); Vacchi and La Mesa (1995); Vacchi et al. (2000)",
                              "T. nicolai",   36,                                                                                                                                        "DeWitt et al. (1990)",
                              "T. peninsulae",   17,                                                                                                                        "Daniels (1981); DeWitt et al. (1990)",
                              "T. pennellii", 25.5,                                                                                                            "DeWitt et al. (1990); Vacchi et al. (1994; 2000)",
                              "T. scotti", 19.7,                                                                                                                         "DeWitt et al. (1990); Miller (1993)",
                              "T. tokarevi", 22.4,                                                                                                                                        "DeWitt et al. (1990)",
                              "T. vicarius",   35,                                                                                                                                        "DeWitt et al. (1990)",
                              "L. larseni",   24,                                                                                                                                        "DeWitt et al. (1990)",
                              "L. mizops",   18,                                                                                                                 "DeWitt et al. (1990); Duhamel et al. (2005)",
                              "L. nudifrons", 19.4,                                                                                                                  "DeWitt et al. (1990); Casaux et al. (2003)",
                              "L. squamifrons",   54,                                                                                                                 "DeWitt et al. (1990); Duhamel et al. (2005)",
                              "Patagonotothen brevicauda",   22,                                                                                                                                      "Moreno and Jara (1984)",
                              "P. canina",   20,                                                                                                                                               "Norman (1937)",
                              "P. cornucola", 26.5,                                                                                                                               "Hart (1946); Hüne et al. 2018",
                              "P. elegans",   12,                                                                                                                                                 "Hart (1946)",
                              "P. guntheri",   24,                                                                                                                 "DeWitt et al. (1990); Collins et al. (2008)",
                              "P. jordani",   21,                                                                                                                                                 "Hart (1946)",
                              "P. kreffti", 37.7,                                                                                                                               "Balushkin and Stehmann (1993)",
                              "P. longipes", 27.7,                                                                                                          "Moreno and Jara (1984); Murillo and Oyarzún (2002)",
                              "P. ramsayi",   47,                                                                                "Laptikhovsky and Fetisov (1999); Brickle et al. (2006); Winter et al. (2010)",
                              "P. sima",   14,                                                                                                                                                 "Hart (1946)",
                              "P. squamiceps",   12,                                                                                                                                               "Norman (1937)",
                              "P. tessellata",   37,                                                                                                  "Hart (1946); Fernández et al. (2012); Hüne and Vega (2015)",
                              "P. thompsoni",   19,                                                                                                                                            "Balushkin (1993)",
                              "P. wiltoni",   34,                                                                                                                                                 "Hart (1946)",
                              "Notothenia trigramma",   28,                                                                                                                                 "Regan (1913); Norman (1937)",
                              "Gobionotothen acuta",   35,                                                                                                                                        "DeWitt et al. (1990)",
                              "G. barsukovi", 26.1,                                                                                                            "Balushkin 1991; A.V. Balushkin pers. com. (2018)",
                              "G. gibberifrons",   55,                                                                                                                                        "DeWitt et al. (1990)",
                              "G. marionensis",   20,                                                                                                                                        "DeWitt et al. (1990)",
                              "Notothenia angustata", 53.4,                                                                                                               "Stewart (2015b); A. Stewart pers. com. (2018)",
                              "N. coriiceps",   65,                                                                                   "DeWitt et al. (1990); Jones et al. (2008 p. 1490) specimen from Bouvetøya",
                              "N. cyanobrancha",   30,                                                                                                                 "DeWitt et al. (1990); Duhamel et al. (2005)",
                              "N. dewitti", 23.8,                                                                                                                                            "Balushkin (1990)",
                              "N. magellanica", 41.3,                                                                                        "DeWitt et al. (1990); Duhamel et al. (2005); Fernández et al. (2012)",
                              "N. microlepidota", 69.5,                                                                                                               "Stewart (2015b); A. Stewart pers. com. (2018)",
                              "N. rossii",   90,                                                                                                                                       "Duhamel et al. (2005)",
                              "Harpagifer andriashevi",  7.8,                                                                                                                                            "Prirodina (2000)",
                              "H. antarcticus",   11,                                                                                                                                "Hureau (1990); Casaux (1998)",
                              "H. bispinis",  7.8,                                                                                                "Hureau (1990); Fernández et al. (2012); Hüne and Vega (2016)",
                              "H. crozetensis",  9.2,                                                                                                                                            "Prirodina (2004)",
                              "H. georgianus",    7,                                                                                                                              "Williams (1988); Hureau (1990)",
                              "H. kerguelensis",  8.3,                                                                                                                        "Hureau (1990); Duhamel et al. (2005)",
                              "H. macquariensis",  7.4,                                                                                                                                            "Prirodina (2000)",
                              "H. nybelini",  5.7,                                                                                                        "Prirodina (2002); O. Voskoboinikov pers. com. (2018)",
                              "H. permitini",  9.8,                                                                                                                                "Neyelov and Prirodina (2006)",
                              "H. palliolatus",   10,                                                                                                                          "Norman (1937 p. 95); Hureau (1990)",
                              "H. spinosus",   10,                                                                                                                        "Hureau (1990); Duhamel et al. (2005)",
                              "Artedidraco glareobarbatus", 13.5,                                                                                                                                    "Eastman and Eakin (1999)",
                              "A. longibarbatus",  9.3,                                                                                                                                         "Eakin et al. (2015)",
                              "A. loennbergi", 11.1,                                                                                                   "Eakin (1990); Olaso et al. (2000); Lombarte et al. (2003)",
                              "A. mirus", 12.5,                                                                                                                                                "Eakin (1990)",
                              "A. orianae", 16.8,                                                                               "Eakin (1990); Olaso et al. (2000); Lombarte et al. (2003); Eastman pers. obs.",
                              "A. shackletoni", 14.6,                                                                                                                                                "Eakin (1990)",
                              "A. skottsbergi",   12,                                                                                                   "Eakin (1990); Olaso et al. (2000); Lombarte et al. (2003)",
                              "Dolloidraco longedorsalis", 13.7,                                                                                                                                                "Eakin (1990)",
                              "Histiodraco velifer", 21.8,                                                                                                                         "Eakin (1990); La Mesa et al. (2006)",
                              "Pogonophryne albipinna",  4.8,                                                                                                                                                "Eakin (1990)",
                              "P. barsukovi",   25,                                                                                                                                                "Eakin (1990)",
                              "P. bellingshausenensis", 23.4,                                                                                                                                         "Eakin et al. (2008)",
                              "P. brevibarbata", 30.1,                                                                                                                                     "Balushkin et al. (2010)",
                              "P. cerebropogon",   31,                                                                                                                                    "Eakin and Eastman (1998)",
                              "P. dewitti",  6.7,                                                                                                                                                "Eakin (1990)",
                              "P. eakini", 24.5,                                                                                                                                            "Balushkin (1999)",
                              "P. favosa", 28.8,                                                                                                                              "Balushkin and Korolkova (2013)",
                              "P. fusca", 17.9,                                                                                                                                  "Balushkin and Eakin (1998)",
                              "P. immaculata", 26.8,                                                                                                                           "Eakin (1990); Eakin et al. (2009)",
                              "P. lanceobarbata",   25,                                                                                                                                                "Eakin (1990)",
                              "P. macropogon",   37,                                                                                                                            "Eakin (1990); Olaso et al (2000)",
                              "P. maculiventrata", 20.3,                                                                                                                              "Spodareva and Balushkin (2014)",
                              "P. marmorata", 24.2,                                                                                                                        "Eakin (1990); Lombarte et al. (2003)",
                              "P. mentella", 22.5,                                                                                                               "Gosse (1966); Andriashev (1967); Eakin (1990)",
                              "P. neyelovi", 35.3,                                                                                                                                  "Shandikov and Eakin (2013)",
                              "P. orangiensis", 20.3,                                                                                                                                  "Eakin and Balushkin (1998)",
                              "P. pavlovi", 27.2,                                                                                                                                            "Balushkin (2013)",
                              "P. permitini",   21,                                                                                                                                                "Eakin (1990)",
                              "P. platypogon",    8,                                                                                                                                                "Eakin (1990)",
                              "P. sarmentifera",   33,                                                                                                                             "Balushkin and Spodareva (2013a)",
                              "P. scotti",   32,                                                                                                                                                "Eakin (1990)",
                              "P. skorai", 19.5,                                                                                                                             "Balushkin and Spodareva (2013b)",
                              "P. stewarti", 24.8,                                                                                                                                         "Eakin et al. (2009)",
                              "P. squamibarbata",   19,                                                                                                                                  "Eakin and Balushkin (2000)",
                              "P. tronio",   33,                                                                                                                                     "Shandikov et al. (2013)",
                              "P. ventrimaculata",   26,                                                                                                                                                "Eakin (1990)",
                              "Gerlachea australis",   28,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "Cygnodraco mawsoni", 55.4,                                                                                                                            "Gon (1990); Vacchi et al. (2000)",
                              "Parachaenichthys charcoti",   53, "Gon (1990); Casaux et al. (2003); Barrera-Oro and Lagger 2010; Balushkin and Voskoboinikova (2011); La Mesa et al. (2012); E. Barrera-Oro pers. com. (2018)",
                              "P. georgianus",   60,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "Racovitzia glacialis", 31.5,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "Prionodraco evansii", 16.5,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "Vomeridens infuscipinnis", 26.5,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "Akarotaxis nudiceps",   15,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "Bathydraco antarcticus", 31.2,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "B. joannae",   24,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "B. macrolepis", 28.2,                                                                                   "Gon (1990); Balushkin and Voskoboinikova (2011); La Mesa et al. (2018a;b)",
                              "B. marri",   26,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "B. scotiae",   20,                                                                                                               "Gon 1990; Balushkin and Voskoboinikova (2011)",
                              "Gymnodraco acuticeps", 42.3,                                                                                                                            "Gon (1990); Vacchi et al. (2000)",
                              "Psilodraco breviceps",   24,                                                                                                             "Gon (1990); Balushkin and Voskoboinikova (2011)",
                              "Acanthodraco dewitti", 18.4,                                                                                                                                                "Skóra (1995)",
                              "Dacodraco hunteri",   29,                                                                                                                                       "Iwami and Kock (1990)",
                              "Champsocephalus esox",   35,                                                                                                                                       "Iwami and Kock (1990)",
                              "C. gunnari",   68,                                                                                                                          "Iwami and Kock (1990); Kock (2005)",
                              "Pagetopsis macropterus",   33,                                                                                                                                       "Iwami and Kock (1990)",
                              "P. maculatus",   25,                                                                                                                                       "Iwami and Kock (1990)",
                              "Neopagetopsis ionah", 57.5,                                                                                                            "Iwami and Kock (1990); Eastman and Hubold (1999)",
                              "Pseudochaenichthys georgianus",   60,                                                                                                                            "Iwami and Kock (1990)",
                              "Chaenodraco wilsoni",   43,                                                                                                                                       "Iwami and Kock (1990)",
                              "Chionodraco hamatus", 50.4,                                                                                                                 "Iwami and Kock (1990); Vacchi et al. (1996)",
                              "C. myersi",   39,                                                                                                                          "Iwami and Kock (1990); Kock (2005)",
                              "C. rastrospinosus",   52,                                                                                                                                       "Iwami and Kock (1990)",
                              "Chaenocephalus aceratus",   76,                                                                                  "Iwami and Kock (1990); Kock et al. (2000); Kock (2005); Reid et al. (2007)",
                              "Channichthys rhinoceratus",   58,                                                                                                                                       "Iwami and Kock (1990)",
                              "Chionobathyscus dewitti",   60,                                                                                                                                       "Iwami and Kock (1990)",
                              "Cryodraco antarcticus",   65,                                                                                                    "Iwami and Kock (1990); Kock and Jones(2002); Kock (2005)",
                              "C. atkinsoni",   51,                                                                                                                                   "La Mesa and Vacchi (1997)")

        ## full genus name
        lastgen <- NA_character_
        genus_maps <- tibble(from = character(), to = character())
        for (n in seq_len(nrow(tempeq))) {
            this_split <- strsplit(tempeq$sp[n], " ")[[1]]
            if (length(this_split) != 2) stop("length != 2")
            if (grepl("\\.", this_split[1])) {
                if (is.na(lastgen) || substr(this_split[1], 1, 1) != substr(lastgen, 1, 1)) {
                    if (this_split[1] == "L.") {
                        lastgen <- "Lepidonotothen"
                    } else {
                        stop("can't figure out full genus name")
                    }
                }
                genus_maps <- rbind(genus_maps, tibble(from = this_split[1], to = lastgen))
                this_split[1] <- lastgen
            }
            tempeq$sp[n] <- paste(this_split, collapse = " ")
            lastgen <- this_split[1]
        }
        ## check
        ## genus_maps %>% distinct %>% dplyr::arrange(from)
        ## remap some names that have different matches in marinespecies.org
        tempeq$sp[tempeq$sp == "Trematomus amphitreta"] <- "Cryothenia amphitreta"
        tempeq$sp[tempeq$sp == "Trematomus peninsulae"] <- "Cryothenia peninsulae"
        tempeq$sp[tempeq$sp == "Notothenia dewitti"] <- "Paranotothenia dewitti"
        ## find Aphia IDs
        nms <- lapply(tempeq$sp, function(nm) tryCatch(worrms::wm_records_names(nm), error = function(e) NULL))
        if (any(sapply(nms, length) > 1)) stop("ambiguous name(s)")
        numeric_or_NA <- function(z) if (is.null(z)) NA_integer_ else z
        char_or_NA <- function(z) if (is.null(z)) NA_character_ else z
        tempeq$aphia_id <- sapply(nms, function(z) numeric_or_NA(z[[1]]$AphiaID))
        tempeq$valid_name <- sapply(nms, function(z) char_or_NA(z[[1]]$valid_name))
        tempeq$valid_aphia_id <- sapply(nms, function(z) numeric_or_NA(z[[1]]$valid_AphiaID))
        ## and map those changed ones back so that we have Eastman's published names
        tempeq$sp[tempeq$sp == "Cryothenia amphitreta"] <- "Trematomus amphitreta"
        tempeq$sp[tempeq$sp == "Cryothenia peninsulae"] <- "Trematomus peninsulae"
        tempeq$sp[tempeq$sp == "Paranotothenia dewitti"] <- "Notothenia dewitti"
        saveRDS(tempeq, file = processed_data_file)
    } else {
        tempeq <- readRDS(processed_data_file)
    }
    ## return all equations in list
    ## include both name and valid_name as separate records, if they are different
    out <- list()
    for (ii in seq_len(nrow(tempeq))) {
        this <- tempeq[ii, ]
        ## catch cases where we don't have an Aphia ID
        tnm <- if (is.na(this$valid_name)) this$sp else this$valid_name
        ## use aphia ID in the identifier if we can, otherwise taxon name
        this_eq_name <- paste0(if (is.na(this$valid_aphia_id)) gsub(" ", "_", tnm) else this$valid_aphia_id, "_maxTL_East2019")
        out[[this_eq_name]] <- list(id = this_eq_name,
                                    taxon_name = if (!is.na(this$valid_name)) this$valid_name else this$sp,
                                    taxon_aphia_id = this$valid_aphia_id,
                                    equation = eval(parse(text = paste0("function(...) tibble(allometric_value = ", this$tl, ")"))),
                                    inputs = tibble(property = character(),units = character()),
                                    return_property = "maximum total length",
                                    return_units = "cm",
                                    notes = paste0("Eastman (2019) cites: ", this$ref),
                                    reference=refs$East2019)
        ## if the name used by Eastman differs from the valid name, enter both
        if (!is.na(this$aphia_id) && (this$valid_aphia_id != this$aphia_id || this$sp != this$valid_name)) {
            cat("Duplicating", this$valid_name, "equation for", this$sp, "\n")
            this_eq_name <- paste0(if (this$aphia_id != this$valid_aphia_id) this$aphia_id else gsub(" ", "_", this$sp), "_maxTL_East2019")
            out[[this_eq_name]] <- list(id = this_eq_name,
                                        taxon_name = this$sp,
                                        taxon_aphia_id = this$aphia_id,
                                        equation = eval(parse(text = paste0("function(...) tibble(allometric_value = ", this$tl, ")"))),
                                        inputs = tibble(property = character(),units = character()),
                                        return_property = "maximum total length",
                                        return_units = "cm",
                                        notes = paste0("Accepted taxon name is ", this$valid_name, ". Eastman (2019) cites: ", this$ref),
                                        reference=refs$East2019)
        }
    }
    out
}
