library(gdxrrw)

x <- rgdx.param(
  "//SY_3600_01/atfs02/Individual/hsakata/AIMPSY/output/biopotential_check/Global/2609022141/main/merged_output.gdx",
  "iamc_gdx"
)

cat("columns:", paste(names(x), collapse = ", "), "\n")
keep <- as.character(x$Sc) %in% c("300C-R31", "300C-R272")
z <- x[keep, ]
cat("rows:", nrow(z), "\n")
cat("scenarios:\n")
print(table(as.character(z$Sc), useNA = "ifany"))
cat("years:\n")
print(table(as.character(z$Y5), useNA = "ifany"))
cat("R31-prefixed regions:", sum(grepl("^R31-", as.character(z$Sr))), "\n")

vars <- c(
  "Cap_Ele_Solar", "Cap_Ele_Win", "Cap_Ele_Hyp", "Cap_Ele_Sto_Bat",
  "Cap_Hyd_Ele", "Sec_Ene_Ele_Cur", "Sec_Ene_Ele_Tra_Los",
  "Sec_Ene_Ele_Tra_Los_Dis", "Sec_Ene_Ele_Tra_Los_Tra", "Sec_Ene_Hyd",
  "Sec_Ene_Hyd_Ele", "Sec_Ene_Inp_Ele_Hyd"
)
q <- z[
  as.character(z$Sv) %in% vars &
    as.character(z$Sr) %in% c("JPN", "CHN", "IND"),
]
print(with(q, table(as.character(Sc), as.character(Sv), as.character(Y5))))

cat("sample duplicate rows:\n")
print(
  q[
    as.character(q$Sc) == "300C-R31" &
      as.character(q$Sr) == "JPN" &
      as.character(q$Sv) == "Cap_Ele_Solar" &
      as.character(q$Y5) == "2050",
  ]
)

cat("raw/factor classes:\n")
print(vapply(z, function(column) paste(class(column), collapse = "/"), character(1)))
