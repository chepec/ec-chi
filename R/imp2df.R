#' Read impedance data from CHI760
#'
#' This function converts CHI760 impedance data to dataframe while retaining metadata.
#' Reads: AC impedance, Impedance - Potential, and datafiles from related techniques
#'
#' @param datafilename  complete path to txt file
#' @param wearea        WE area in sqcm, defaults to NA
#'
#' @return dataframe with data and metadata
#' @export
imp2df <- function(datafilename, wearea = NA) {
   # n = -1 to read all lines
   filecontents <- readLines(datafilename, n = -1)
   # remove all empty rows (makes it easier to identify metadata/data later)
   filecontents <- filecontents[-which(filecontents == "")]

   # identify the start of the data block
   # (using a regexp that matches scientific notation with optional negative sign)
   rows.of.data <- regexpr("^(-?\\d+)\\.?\\d+(e-|e\\+|e|\\d+)\\d+", filecontents)
   # http://stackoverflow.com/questions/638565/parsing-scientific-notation-sensibly
   # http://stackoverflow.com/questions/4479408/regex-for-numbers-on-scientific-notation

   # throw away the attributes (we don't need them)
   attributes(rows.of.data) <- NULL
   rownumbers.of.data <- which(rows.of.data == 1)
   # there's always a header line, and I like this to be remain with the data
   #rownumbers.of.data <- c(min(rownumbers.of.data) - 1, rownumbers.of.data)

   # Note: don't read the header row into read.csv -- the escaped prime or double-prime chars
   #       in the header causes a weird bug that silently drops several rows of data
   ff <- utils::read.csv(textConnection(filecontents[rownumbers.of.data]), header = FALSE)
   # it's hard to read colnames directly from source because
   # Z-prime and double-prime are automatically converted to meaningless dots
   # plus the double-prime (quote mark) seriously messes up read.csv() ...
   # The first column usually varies, but the rest of them appears to be the same
   # for all impedance techniques I have tested so far.
   names(ff) <-
      c(sub("/.*$", "", filecontents[rownumbers.of.data[1] - 1]),
        "ReZ", # real (prime)
        "ImZ", # imaginary (double-prime)
        "Z",
        "Phase")
   # add sampleid column (try to get just the sampleid without the rest of the filename)
   ff$sampleid <-
      common::ProvideSampleId(datafilename) %>% sub("-.*$", "", .)

   # the WE area
   ff$area <- wearea

   # Now read and save metadata
   metadataraw <- filecontents[which(rows.of.data == -1)]
   # The first row of the file is always date and time
   ff$datetime <- lubridate::mdy_hms(metadataraw[1])
   # The second row is always the name of the technique
   ff$technique <- metadataraw[2]

   # Collect all metadata headers that occur between the row "Note:" until the header row
   metadata.header.rows <-
      seq(grep("^Note:", metadataraw) + 1, length(metadataraw) - 1)
   # all rows in metadata.header.rows should now look like this
   # "Amplitude (V) = 0.005"
   # (the numeric part could also be in scientific notation)
   # Let's split along the equals sign, save the label as colname (without unit)
   metadata.headers <- metadataraw[metadata.header.rows]
   metadata.table <- utils::read.table(textConnection(metadata.headers), sep = "=")
   # read the metadata section (labels = value) with arbitrary number of lines,
   # clean up the labels (remove trailing spaces),
   # (note that read.table automatically makes the value column numeric)
   # we will also split off the units into their own column, just because
   # (but we are not doing anything with them for the time being)
   metadata.table$V3 <-
      utils::read.table(textConnection(gsub(")\\s+$", "", metadata.table$V1)), sep = "(")[, 2]
   metadata.table$V1 <-
      sub("\\s+$", "", utils::read.table(textConnection(gsub(")\\s+$", "", metadata.table$V1)), sep = "(")[, 1])
   metadata.table$V1 <- make.names(metadata.table$V1)
   # later we use spread() to transpose this df and to retain the order of the columns
   # we will define explicit levels
   # (otherwise spread() sorts the columns alphabetically, which is a little confusing)
   metadata.table$V1 <-
      factor(metadata.table$V1, levels = metadata.table$V1)
   # now transpose the metadata df and add a sampleid col, and we'll be ready to join ff
   metadata.df <-
      metadata.table %>%
      # we don't want the unit column to complicate matters...
      dplyr::select(-V3) %>%
      dplyr::mutate(group = 1) %>%
      tidyr::spread(V1, V2) %>%
      # drop the added "group" column
      dplyr::select(-group)
   metadata.df$sampleid <- unique(ff$sampleid)
   df <- dplyr::right_join(ff, metadata.df, "sampleid")

   return(df)
}
