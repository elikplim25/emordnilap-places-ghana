# ============================================================================
#  data.R  --  load + enrich the Kings dataset
#  Everything the app needs is built once, at start-up, from:
#    data/Kings data__CLAUDE.xlsx        (YOUR spreadsheet - the source of truth)
#    data/chronology_reference.csv       (approximate BC dates, used to PLACE kings on a timeline)
#    data/secondary_synchronisms.csv     (the 3 "second" synchronisms mentioned in your Notes)
#    data/cross_links.csv                (alliances, wars, marriages, coups between kings)
# ============================================================================

KINGDOM_LEVELS <- c("United", "Northern", "Southern")
KINGDOM_LABELS <- c(United = "United Kingdom", Northern = "Israel (North)", Southern = "Judah (South)")
KINGDOM_SHORT  <- c(United = "United", Northern = "Israel (North)", Southern = "Judah (South)")   # for narrow side-strips
VERDICT_LEVELS <- c("Good", "Mixed", "Evil", "N/A")

# palette -------------------------------------------------------------------
# colours come from the {ltc} package: "heatmap0" (verdicts, ink, page) and "hat" (kingdoms, ties)
LTC_HEATMAP0 <- c("#001219", "#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00", "#CA6702", "#AE2012", "#9B2226")
LTC_HAT      <- c("#efb306", "#eb990c", "#e8351e", "#cd023d", "#852f88", "#4e54ac", "#0f8096", "#7db954", "#17a769", "#000000")
PAL <- list(
  ink      = "#001219",     # heatmap0 #1  - ALL text
  ink_soft = "#005F73",     # heatmap0 #2  - headings / secondary text (still strong)
  paper    = "#F6F0DC",     # page: heatmap0 sand, lightened
  card     = "#FFFFFF",
  rule     = "#D8CDA6",
  sand     = "#E9D8A6",
  kingdom  = c(United = "#852f88", Northern = "#4e54ac", Southern = "#17a769"),    # hat
  verdict  = c(Good = "#005F73", Mixed = "#EE9B00", Evil = "#AE2012", `N/A` = "#6B7A80"),   # heatmap0
  verdict_txt = c(Good = "#FFFFFF", Mixed = "#001219", Evil = "#FFFFFF", `N/A` = "#FFFFFF"),
  tie      = c(`Kin / marriage` = "#852f88", Alliance = "#0A9396", `War / rivalry` = "#EE9B00", `Killed / overthrown` = "#AE2012"),
  rel      = c(`Family` = "#005F73", `Not related` = "#EE9B00", `First king` = "#94D2BD")
)
KINGDOM_TINT <- c(United = "#EBD7EC", Northern = "#DCDDF0", Southern = "#D4EFE2")
DYN_COLS <- c("#efb306", "#e8351e", "#852f88", "#4e54ac", "#0f8096", "#7db954", "#CA6702", "#0A9396", "#cd023d", "#17a769", "#9B2226", "#005F73", "#eb990c", "#94D2BD")

# helpers -------------------------------------------------------------------
reign_short <- function(y) {
  dplyr::case_when(y <= 0.03 ~ "7d", y <= 0.09 ~ "1m", y <= 0.26 ~ "3m", y <= 0.51 ~ "6m",
                   TRUE ~ paste0(format(y, trim = TRUE, drop0trailing = TRUE), "y"))
}
fmt_reign <- function(y) {
  dplyr::case_when(
    is.na(y)        ~ "-",
    y <= 0.03       ~ "7 days",
    y <= 0.09       ~ "1 month",
    y <= 0.26       ~ "3 months",
    y <= 0.51       ~ "6 months",
    y == 1          ~ "1 year",
    TRUE            ~ paste(format(y, trim = TRUE, drop0trailing = TRUE), "years")
  )
}
ord <- function(n) {
  n <- as.integer(n)
  suf <- ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")[n %% 10 + 1])
  paste0(n, suf)
}
as_text <- function(x) ifelse(is.na(x), "", as.character(x))

# the sync-partner lookup (text in the sheet -> king id) --------------------
SYNC_PARTNER_ID <- c(
  "Asa (Judah)" = 26, "Jehoshaphat (Judah)" = 27, "Joash (Judah)" = 31, "Amaziah (Judah)" = 32,
  "Uzziah (Judah)" = 33, "Ahaz (Judah)" = 35,
  "Jeroboam I (Israel)" = 5, "Ahab (Israel)" = 11, "Joram (Israel)" = 13, "Jehu (Israel)" = 14,
  "Jehoash (Israel)" = 16, "Jeroboam II (Israel)" = 17, "Pekah (Israel)" = 22, "Hoshea (Israel)" = 23
)

# ---------------------------------------------------------------------------
load_kings <- function(dir = "data") {
  xl <- list.files(dir, pattern = "^Kings.*\\.xlsx$", full.names = TRUE)
  if (!length(xl)) stop("Put your 'Kings data' .xlsx file in the app's data/ folder.", call. = FALSE)
  raw <- readxl::read_excel(xl[1], sheet = 1)
  names(raw)[1:14] <- c("id", "name", "kingdom_raw", "house_raw", "years", "age_start", "age_end",
                        "sync_year", "sync_with", "verdict", "rel_prev", "refs", "notes", "prophets_raw")
  raw <- raw[!is.na(raw$id), ]

  chron <- utils::read.csv(file.path(dir, "chronology_reference.csv"), stringsAsFactors = FALSE)

  k <- raw |>
    dplyr::mutate(
      id         = as.integer(id),
      kingdom    = factor(dplyr::case_when(
        grepl("United", kingdom_raw)   ~ "United",
        grepl("Northern", kingdom_raw) ~ "Northern",
        TRUE                           ~ "Southern"), levels = KINGDOM_LEVELS),
      verdict    = factor(trimws(verdict), levels = VERDICT_LEVELS),
      rel_prev   = trimws(sub("\\s*\\(.*\\)\\s*$", "", rel_prev)),     # "Servant (Not related by blood)" -> "Servant"
      rel_prev   = ifelse(rel_prev == "Not related" | rel_prev == "None" | rel_prev == "Servant", rel_prev, rel_prev),
      sync_with  = trimws(sync_with),
      # short display name: first alias, no (Israel)/(Judah) tag
      short      = trimws(sub("\\s*\\((Israel|Judah)\\)\\s*$", "", name)),
      short      = trimws(sub("\\s*/.*$", "", short)),
      short      = trimws(sub("\\s*\\(Eshbaal\\)", "", short))
    ) |>
    dplyr::left_join(chron, by = "id")

  # disambiguate duplicate short names (Ahaziah, Jehoahaz)
  dup <- k$short[duplicated(k$short) | duplicated(k$short, fromLast = TRUE)]
  k$label <- ifelse(k$short %in% dup,
                    paste0(k$short, " (", ifelse(k$kingdom == "Southern", "Judah", "Israel"), ")"), k$short)

  # houses / dynasties --------------------------------------------------------
  first_name <- trimws(sub("\\s+I{1,3}$", "", k$short))
  k$house <- dplyr::case_when(
    k$house_raw == "Founder"  ~ paste("House of", first_name),
    k$house_raw == "Usurper"  ~ paste0("Usurper (", k$short, ")"),
    TRUE                      ~ k$house_raw
  )
  k$is_usurper <- k$house_raw == "Usurper"

  # relationship classes --------------------------------------------------------
  k$related <- k$rel_prev %in% c("Son", "Brother", "Grandson", "Mother", "Uncle")
  k$rel_class <- dplyr::case_when(
    k$rel_prev == "None"        ~ "First king",
    k$related                   ~ "Related",
    TRUE                        ~ "Not related"
  )

  k$reign_txt <- fmt_reign(k$years)
  k$span_txt  <- paste0(k$start_bc, if_else_chr(k$start_bc == k$end_bc, "", paste0("-", k$end_bc)), " BC")
  k$span_txt[is.na(k$start_bc)] <- "-"
  k$sync_partner_id <- unname(SYNC_PARTNER_ID[k$sync_with])
  if (any(!is.na(k$sync_with) & is.na(k$sync_partner_id)))
    warning("Unrecognised 'Synchronized With' text: ",
            paste(unique(k$sync_with[!is.na(k$sync_with) & is.na(k$sync_partner_id)]), collapse = "; "))

  # prophets ----------------------------------------------------------------
  pr <- strsplit(ifelse(is.na(k$prophets_raw), "", k$prophets_raw), "\\s*,\\s*")
  k$prophets <- lapply(pr, function(v) v[!(v %in% c("", "None recorded")) & !grepl("^Unnamed", v)])
  k$unnamed  <- vapply(pr, function(v) any(grepl("^Unnamed", v)), logical(1))
  k$n_prophets <- lengths(k$prophets)

  k <- k[order(k$id), ]
  k
}

if_else_chr <- function(cond, a, b) ifelse(cond, a, b)

# ---------------------------------------------------------------------------
# synchronism table: one row per stated link, with a reference-chronology check
# ---------------------------------------------------------------------------
build_sync <- function(k, dir = "data") {
  prim <- k[!is.na(k$sync_partner_id), c("id", "sync_year", "sync_partner_id", "sync_with")]
  prim$kind <- "primary"
  prim$ref  <- NA_character_
  sec <- utils::read.csv(file.path(dir, "secondary_synchronisms.csv"), stringsAsFactors = FALSE)
  sec <- data.frame(id = sec$id, sync_year = sec$year, sync_partner_id = sec$partner_id,
                    sync_with = k$name[match(sec$partner_id, k$id)], kind = "secondary", ref = sec$ref,
                    stringsAsFactors = FALSE)
  s <- rbind(prim, sec)
  s$partner_start <- k$start_bc[match(s$sync_partner_id, k$id)]
  s$own_start     <- k$start_bc[match(s$id, k$id)]
  # where on the PARTNER's reign does the text say this king began?  (partner start - (N-1) years)
  s$claimed_bc <- s$partner_start - (s$sync_year - 1)
  # Jeroboam I's row reads the other way round: it is Jeroboam's OWN 20th year when Asa began
  rev <- s$id == 5 & s$kind == "primary"
  s$claimed_bc[rev] <- s$own_start[rev] - (s$sync_year[rev] - 1)
  s$gap_years <- ifelse(rev, s$claimed_bc - s$partner_start, s$own_start - s$claimed_bc)
  s$agrees <- abs(s$gap_years) <= 3
  s$text <- ifelse(
    rev,
    paste0("Asa of Judah began in the ", ord(s$sync_year), " year of Jeroboam I"),
    paste0("Began in the ", ord(s$sync_year), " year of ", s$sync_with)
  )
  s
}

# ---------------------------------------------------------------------------
# kings whose reigns overlapped (reference chronology), across kingdoms
# ---------------------------------------------------------------------------
build_overlaps <- function(k) {
  w <- data.frame(id = k$id, k = as.character(k$kingdom), s = k$start_bc,
                  e = pmin(k$end_bc, k$start_bc - 0.5))
  out <- list()
  for (i in seq_len(nrow(w))) for (j in seq_len(nrow(w))) {
    if (i == j || w$k[i] == w$k[j]) next
    ov <- min(w$s[i], w$s[j]) - max(w$e[i], w$e[j])
    if (ov >= 0.5) out[[length(out) + 1]] <- data.frame(id = w$id[i], other = w$id[j], overlap = round(ov, 1))
  }
  do.call(rbind, out)
}

load_links <- function(k, dir = "data") {
  l <- utils::read.csv(file.path(dir, "cross_links.csv"), stringsAsFactors = FALSE)
  l$scope <- ifelse(as.character(k$kingdom[match(l$from_id, k$id)]) ==
                      as.character(k$kingdom[match(l$to_id, k$id)]), "within", "between")
  l$link_id <- seq_len(nrow(l))
  l
}

# ---------------------------------------------------------------------------
# tidy prophet table
# ---------------------------------------------------------------------------
build_prophets <- function(k) {
  rows <- lapply(seq_len(nrow(k)), function(i) {
    p <- k$prophets[[i]]
    if (!length(p)) return(NULL)
    data.frame(prophet = p, king_id = k$id[i], stringsAsFactors = FALSE)
  })
  p <- do.call(rbind, rows)
  p$kingdom <- k$kingdom[match(p$king_id, k$id)]
  p$start_bc <- k$start_bc[match(p$king_id, k$id)]
  p$end_bc   <- k$end_bc[match(p$king_id, k$id)]
  p
}

`%||%` <- function(a, b) if (is.null(a)) b else a
