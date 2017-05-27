install.packages("tabulizer")
install.packages("dplyr")
install.packages("pdftables")
install.packages("pdftools")
install.packages("readr")
install.packages("stringr")
install.packages("magrittr")
install.packages("XML")

library(tabulizer)
library(dplyr)
library(pdftables)
library(pdftools)
library(readr)
library(stringr)
library(magrittr)
library(RGoogleDocs)
library(XML)

# pull list of current files
files <- list.files(path = "financials")

# table headers for each table
tables_list <- c("POSITIONS","ASSETS","AGREEMENTS","SOURCES","SPOUSE","OTHER",
                 #"TRANSACTIONS",
                 "LIABILITIES"
                 #,
                 #"GIFTS"
                 )

master_tables_list <- paste0(tables_list,"_all")

header_names <- list(POSITIONS = c("#", "ORGANIZATION NAME", "CITY, STATE", "ORGANIZATION TYPE", "POSITION HELD","FROM","TO"),
                     ASSETS = c("#", "DESCRIPTION", "EIF", "VALUE", "INCOME TYPE","INCOME AMOUNT"),
                     AGREEMENTS = c("#", "EMPLOYER OR PARTY","CITY, STATE", "STATUS AND TERMS", "DATE"),
                     SOURCES = c("#", "SOURCE NAME","CITY, STATE","BRIEF DESCRIPTION OF DUTIES"),
                     SPOUSE = c("#", "DESCRIPTION", "EIF", "VALUE", "INCOME TYPE","INCOME AMOUNT"),
                     OTHER = c("#", "DESCRIPTION", "EIF", "VALUE", "INCOME TYPE","INCOME AMOUNT"),
                     LIABILITIES = c("#", "CREDITOR NAME", "TYPE", "AMOUNT", "YEAR INCURRED", "RATE", "TERM"))

header_names_trunc <- list(POSITIONS = c("#", "ORGANIZATION NAME", "CITY, STATE", "ORGANIZATION  ", "POSITION HELD","FROM","TO"),
                           ASSETS = c("#", "DESCRIPTION", "EIF", "VALUE", "INCOME TYPE","INCOME$"),
                           AGREEMENTS = c("#", "EMPLOYER OR PARTY","CITY, STATE", "STATUS AND TERMS", "DATE"),
                           SOURCES = c("#", "SOURCE NAME","CITY, STATE","BRIEF DESCRIPTION OF DUTIES"),
                           SPOUSE = c("#", "DESCRIPTION", "EIF", "VALUE", "INCOME TYPE","INCOME$"),
                           OTHER = c("#", "DESCRIPTION", "EIF", "VALUE", "INCOME TYPE","INCOME$"),
                           LIABILITIES = c("#", "CREDITOR NAME", "TYPE", "AMOUNT", "YEAR", "RATE", "TERM"))

# create master tables
POSITIONS_all <- setNames(data.frame(matrix(ncol = length(header_names$POSITIONS)+2, nrow = 0)), c(header_names$POSITIONS, "NAME","TABLE"))
ASSETS_all <- setNames(data.frame(matrix(ncol = length(header_names$ASSETS)+2, nrow = 0)), c(header_names$ASSETS, "NAME","TABLE"))
AGREEMENTS_all <- setNames(data.frame(matrix(ncol = length(header_names$AGREEMENTS)+2, nrow = 0)), c(header_names$AGREEMENTS, "NAME","TABLE"))
SOURCES_all <- setNames(data.frame(matrix(ncol = length(header_names$SOURCES)+2, nrow = 0)), c(header_names$SOURCES, "NAME","TABLE"))
SPOUSE_all <- setNames(data.frame(matrix(ncol = length(header_names$SPOUSE)+2, nrow = 0)), c(header_names$SPOUSE, "NAME","TABLE"))
OTHER_all <- setNames(data.frame(matrix(ncol = length(header_names$OTHER)+2, nrow = 0)), c(header_names$OTHER, "NAME","TABLE"))
LIABILITIES_all <- setNames(data.frame(matrix(ncol = length(header_names$LIABILITIES)+2, nrow = 0)), c(header_names$LIABILITIES, "NAME","TABLE"))

for (i in 1:length(files)) {
    
    # extract file name
    name <- paste(substr(files[i],str_locate(pattern = ", ",files[i])[2]+1,str_locate(pattern = ".pdf|.PDF",files[i])[1]-1),
                  substr(files[i],1,str_locate(pattern = ", ",files[i])[1]-1))
    filename_ext <- gsub(".pdf","",gsub(",","",gsub(" ", "", files[i])))
    
    # load pdf as text and unlist
    text <- pdf_text(paste0("financials/",files[i]))
    table_data <- str_split(text, pattern = "\n")
    table_data_unlisted <- unlist(table_data)
    
    # define coordinates of table sections
    row_starts <- c(match("1. Filer's Positions Held Outside United States Government", table_data_unlisted)+1,
                    match("2. Filer's Employment Assets & Income and Retirement Accounts", table_data_unlisted)+1,
                    match("3. Filer's Employment Agreements and Arrangements", table_data_unlisted)+1,
                    match("4. Filer's Sources of Compensation Exceeding $5,000 in a Year", table_data_unlisted)+1,
                    match("5. Spouse's Employment Assets & Income and Retirement Accounts", table_data_unlisted)+1,
                    match("6. Other Assets and Income", table_data_unlisted)+1,
                    # match("7. Transactions", table_data_unlisted)+1,
                    match("8. Liabilities", table_data_unlisted)+1
                    #,
                    # match("9. Gifts and Travel Reimbursements", table_data_unlisted)+1
                    )
   
     row_ends <- c(match("2. Filer's Employment Assets & Income and Retirement Accounts", table_data_unlisted)-1,
                  match("3. Filer's Employment Agreements and Arrangements", table_data_unlisted)-1,
                  match("4. Filer's Sources of Compensation Exceeding $5,000 in a Year", table_data_unlisted)-1,
                  match("5. Spouse's Employment Assets & Income and Retirement Accounts", table_data_unlisted)-1,
                  match("6. Other Assets and Income", table_data_unlisted)-1,
                  match("7. Transactions", table_data_unlisted)-1,
                  # match("8. Liabilities", table_data_unlisted)-1,
                  match("9. Gifts and Travel Reimbursements", table_data_unlisted)-1
                  #,
                  # match("Endnotes", table_data_unlisted)-1
                  )
   
    table_coordinates <- data.frame(rbind(row_starts,row_ends))
    colnames(table_coordinates) <- tables_list
    
    # pull data for each table
    for (a in 1:length(tables_list)) {
        table_start <- table_coordinates[,a][1]
        table_end <- table_coordinates[,a][2]
        table_raw <- table_data_unlisted[table_start:table_end]
        split <- which(grepl("ORGANIZATION NAME|SOURCE NAME|CREDITOR NAME|EMPLOYER OR PARTY|DESCRIPTION", table_raw))
        if(length(split) > 0) {
            header_splits <- data.frame()
            table <- setNames(data.frame(matrix(ncol = length(header_names[[a]]), nrow = 0)), header_names[[a]])
            for (j in 1:length(split)) {
                for (k in 1:length(header_names_trunc[[a]])) {
                    header_splits[k,j] <- (str_locate(pattern = header_names_trunc[[a]][k], table_raw[split[j]])[1])
                }
                header_splits[length(header_names_trunc[[a]])+1, j] <- nchar(table_raw[split[j]][1])+1
            }
            table <- setNames(data.frame(matrix(ncol = length(header_names[[a]]), nrow = 0)), header_names[[a]])
            # set offset 
            if (identical(header_names_trunc[[a]], header_names[[a]]) == TRUE) {
                offset <- 1}
            if (identical(header_names_trunc[[a]], header_names[[a]]) == FALSE) {
                offset <- 2}
            
            for (t in 1:ncol(header_splits)) {
                if (t < ncol(header_splits)) {
                    for (r in (split[t]+offset):(split[t+1]-1)){ 
                        for (h in 1:(length(header_splits[,t])-2)) {
                            table[r,h] <- c(substr(table_raw[r], header_splits[h,t], header_splits[h+1,t]-1))
                        }
                        table[r,(length(header_splits[,t])-1)] <- c(substr(table_raw[r], header_splits[(length(header_splits[,t])-1),t], nchar(table_raw[r])))
                    }
                }
                else if (t == ncol(header_splits)) {
                    for (r in (split[t]+offset):length(table_raw)) {
                        for (h in 1:(length(header_splits[,t])-2)) {
                            table[r,h] <- c((substr(table_raw[r], header_splits[h,t], header_splits[h+1,t]-1)))
                        }
                        table[r,(length(header_splits[,t])-1)] <- c(substr(table_raw[r], header_splits[(length(header_splits[,t])-1),t], nchar(table_raw[r])))
                    }
                }
            }
            table[,1] <- trimws(table[,1])
            for (v in 1:ncol(table)) {
                table[,v] <- gsub("\\s+"," ",table[,v])}
            table[is.na(table)] <- " "
            if(grepl("^\\s*$", table[1,1])) {
                table <- table[2:nrow(table),]
            }
            if(grepl("^\\s*$", table[1,1])) {
                table <- table[2:nrow(table),]
            }
            if(grepl("^\\s*$", table[1,1])) {
                table <- table[2:nrow(table),]
            }
            for (n in nrow(table):2) {
                if((nrow(table) > 1) && ((grepl(".*\\.$", table[n-1,1]) || (grepl("^\\..*", table[n,1]))))) {
                    table[n-1,1] <- paste0(table[n-1,1], table[n,1])
                    table[n,1] <- " "
                }
                else next
            }
            for (n in nrow(table):2) {
                if(nrow(table) > 1 && (
                    grepl("^\\s*$", table[n,1]))) {
                    for (p in 1:ncol(table)) {
                        table[n-1,p] <- paste(table[n-1,p], table[n,p])
                    }
                    table <- table[-c(n),]
                }
                else next
            }
            for (n in nrow(table):2) {
                if(nrow(table) > 1 && 
                   (as.integer(sub(pattern = "\\..*", "", table[n,1])) < (as.integer(sub(pattern = "\\..*", "", table[n-1,1])))))  {
                    for (p in 1:ncol(table)) {
                        table[n-1,p] <- paste(table[n-1,p], table[n,p])
                    }
                    table <- table[-c(n),]
                }
                else next
            }
            table$NAME <- rep(name,nrow(table))
            table$TABLE <- rep(tables_list[a],nrow(table))
            if(a == 1) {POSITIONS_all <- rbind(POSITIONS_all, table)}
            if(a == 2) {ASSETS_all <- rbind(ASSETS_all, table)}
            if(a == 3) {AGREEMENTS_all <- rbind(AGREEMENTS_all, table)}
            if(a == 4) {SOURCES_all <- rbind(SOURCES_all, table)}
            if(a == 5) {SPOUSE_all <- rbind(SPOUSE_all, table)}
            if(a == 6) {OTHER_all <- rbind(OTHER_all, table)}
            if(a == 7) {LIABILITIES_all <- rbind(LIABILITIES_all, table)}
        }
    }
}

total_assets_all <- rbind(ASSETS_all, OTHER_all, SPOUSE_all)

write.csv(AGREEMENTS_all,"agreements.csv")
write.csv(ASSETS_all, "assets.csv")
write.csv(LIABILITIES_all,"liabilities.csv")
write.csv(OTHER_all,"other.csv")
write.csv(POSITIONS_all, "positions.csv")
write.csv(SOURCES_all, "sources.csv")
write.csv(SPOUSE_all, "spouse.csv")
write.csv(total_assets_all, "totals.csv")
