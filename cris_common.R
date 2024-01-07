#==============================================================================
# Common functions for CRIS
# Rudolf Cardinal, University of Cambridge / CPFT
# Started: 17 May 2013
# Last modified: 1 Dec 2016
#==============================================================================

# library(ez)
# library(ggplot2)
# library(plyr)
# library(RCurl) # for email
# library(reshape2)
# library(sqldf)
# library(stringr)  # for str_pad
source("miscstat.R") # for half_confidence_interval_t

# For parallel:
# library(foreach)

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

cris <- new.env()

#==============================================================================
# Database connection
#==============================================================================

# configure environment variables:
# CRIS_USER
# CRIS_PASSWORD

cris$connect <- function(
    user=Sys.getenv("CRIS_USER"),
    password=Sys.getenv("CRIS_PASSWORD"),
    server="BRHNSQL009",  # 10.16.32.47
    port=1433,
    method=c("ODBC", "JDBC"),
    odbc_driver="SQL Server",
    jdbc_driver="sqlserver",
    jdbc_driverClass="com.microsoft.sqlserver.jdbc.SQLServerDriver",
    jdbc_classPath="",
    jdbc_identifier.quote=NA)
{
    # Connect to CRIS database, using (by default) login details from
    # environment variables
    method <- match.arg(method)
    if (method == "ODBC") {
        library(RODBC)
        return(odbcDriverConnect(paste('driver={', odbc_driver, '};',
                                       'server=', server, ',', port,
                                       ';UID=', user,
                                       ';PWD=', password, sep="")))
        # WILL SILENTLY USE Trusted_Connection=Yes unless you get this exactly right.
        # In which case, access to the GATE database will fail.
        # Print the dbhandle object to see its connection method.
    } else {
        # JDBC
        library(RJDBC)
        drv <- JDBC(driverClass=jdbc_driverClass,
                    classPath=jdbc_classPath,
                    identifier.quote=jdbc_identifier.quote)
        url <- paste("jdbc:", jdbc_driver, "://", server, ":", port, ";",
                     "user=", user, ";",
                     "password=", password, ";",
                     "responseBuffering=adaptive;",
                     "selectMethod=cursor;",
                     sep="")
        return(dbConnect(drv, url))
    }

    # http://stackoverflow.com/questions/15420999/rodbc-odbcdriverconnect-connection-error
}

# For proxies:
# R: method 1
# - environment variable HTTP_PROXY <- http://user:password@host:port/
# - RStudio - doesn't work
# R: method 2
# - HTTP_PROXY <- http://host:port/
# - HTTP_PROXY_USER <- ask
#   (that's the literal "ask")
# - RStudio - doesn't work
# R: method 3
# - HTTP_PROXY <- http://host:port/
# - HTTP_PROXY_USER <- user:password
# - RStudio - doesn't work
# HOWEVER, RStudio works fine when you DISABLE
#   Tools > Options > Packages > Use Internet Explorer library/proxy for HTTP

# Sys.getenv("HTTP_PROXY")
# Sys.getenv("HTTP_PROXY_USER")

#==============================================================================
# Email
#==============================================================================

cris$send_email <- function(recipients, subject, body, filenames=c())
{
    # DOES NOT WORK - blocked by SLAM firewall.
    EMAIL_SCRIPT <- Sys.getenv("EMAIL_SCRIPT")
    # should point to a local copy of http://egret.psychol.cam.ac.uk/pythonlib/send_email.py
    SMTP_SENDER <- Sys.getenv("SMTP_SENDER")
    SMTP_HOST <- Sys.getenv("SMTP_HOST")
    SMTP_USER <- Sys.getenv("SMTP_USER")
    SMTP_PASSWORD <- Sys.getenv("SMTP_PASSWORD")
    command <- cat(
        EMAIL_SCRIPT
        , SMTP_SENDER
        , SMTP_HOST
        , SMTP_USER
        , SMTP_PASSWORD
        , cat(recipients, sep=",")
        , subject
        , body
        , "--tls"
    )
    if (length(filenames) > 0) {
        command <- cat(command, paste("--attach", filenames))
    }
    # cat(command, "\n")
    system(command)
}


cris$route_email <- function(
    url # specify your custom mail routing server script here
    , sender
    , recipient
    , host
    , user
    , password
    , subject=""
    , body=""
    , filenames=c()
    , proxy=Sys.getenv("HTTP_PROXY")
    , proxyuserpwd=Sys.getenv("HTTP_PROXY_USER")
    , ssl.verifypeer=FALSE # for HTTPS... but avoid HTTPS as it returns an encrypted result!
)
{
    # Uses a routing server, with a custom script, to route e-mails.
    # (This does not represent an open mail gateway; it merely routes it
    # to an appropriate SMTP server.)
    params <- list(
        sender=sender
        , recipient=recipient
        , subject=subject
        , body=body
        , host=host
        , user=user
        , password=password
    )
    for (i in 1:length(filenames)) {
        params[paste("file_", i, sep="")] <- list(
            fileUpload(filename=filenames[i])
        )
        # fileUpload is from RCurl
    }
    style <- "httppost" # for multipart/form-data (required for proper file transmission)
    opts <- list(
        proxy=proxy
        , proxyuserpwd=proxyuserpwd
        , ssl.verifypeer=ssl.verifypeer
        , http.version=HTTP_VERSION_1_0 # or style="httppost" fails
    )
    encoding <- "utf8"
    result <- postForm(
        uri=url
        , .params=params
        , .opts=opts
        , style=style
        , .encoding=encoding
    )
    return(result)
}

#==============================================================================
# Times, dates
#==============================================================================

cris$DAYS_PER_YEAR <- 365.25 # ... on average
cris$SECONDS_PER_DAY <- 24 * 60 * 60
cris$UNIX_EPOCH <- "1970-01-01"

cris$posixct_from_unix_datetime_in_seconds <- function(x)
{
    # When sqldf mangles a DATETIME or POSIXct field to an integer,
    # unmangle it with this
    as.POSIXct(as.integer(x), origin=cris$UNIX_EPOCH)
}

cris$date_from_unix_date_in_days <- function(x)
{
    # When sqldf mangles a DATE or Date field to an integer,
    # unmangle it with this
    as.Date(as.integer(x), origin=cris$UNIX_EPOCH)
}

cris$days_between <- function(start, end)
{
    # Works for POSIXct and Date
    as.numeric(difftime(end, start, units="days"))
}

cris$years_between <- function(start, end)
{
    # Works for POSIXct and Date
    cris$days_between(start, end) / cris$DAYS_PER_YEAR
}

cris$round_date_to_days <- function(d)
{
    round(d)
}

cris$date_halfway_between <- function(start, end)
{
    halfspan <- end - start
    round(start + halfspan)
}

cris$year_from_date <- function(d)
{
    as.numeric(format(d, "%Y"))
}

cris$date_range_inclusive <- function(start_date, end_date)
{
    seq(start_date, end_date, by=1)
}

#==============================================================================
# Drugs
#==============================================================================

cris$FGA <- c(
    "benperidol"
    , "chlorpromazine"
    , "flupentixol"
    , "fluphenazine"
    , "haloperidol"
    , "levomepromazine"
    , "pericyazine"
    , "perphenazine"
    , "pimozide"
    , "pipotiazine"
    , "prochlorperazine"
    , "promazine"
    , "sulpiride"
    , "trifluoperazine"
    , "zuclopenthixol"
)
cris$FGA_DEPOT <- c(
    "haloperidol_depot"
    , "flupentixol_depot"
    , "fluphenazine_depot"
    , "paliperidone_depot"
    , "pipotiazine_depot"
    , "zuclopenthixol_depot"
)
cris$SGA_NOT_CLOZAPINE <- c(
    "amisulpride"
    , "aripiprazole"
    , "asenapine"
    , "iloperidone"
    , "lurasidone"
    , "olanzapine"
    , "paliperidone"
    , "quetiapine"
    , "risperidone"
    , "sertindole"
    , "ziprasidone"
    , "zotepine"
)
cris$SGA <- c(
    cris$SGA_NOT_CLOZAPINE
    , "clozapine"
)
cris$SGA_DEPOT <- c(
    "olanzapine_depot"
    , "risperidone_depot"
)
cris$SSRI <- c(
    "citalopram"
    , "escitalopram"
    , "fluoxetine"
    , "fluvoxamine"
    , "paroxetine"
    , "sertraline"
)
cris$NON_SSRI_MODERN_ANTIDEPRESSANT <- c(
    "agomelatine"
    , "bupropion"
    , "duloxetine"
    , "mirtazapine"
    , "reboxetine"
    , "tryptophan"
    , "venlafaxine"
)
cris$MAOI <- c(
    "phenelzine"
    , "isocarboxazid"
    , "moclobemide" # reversible
    , "tranylcypromine"
)
cris$TCA <- c(
    "amitriptyline"
    , "amitriptyline_with_perphenazine"
    , "clomipramine"
    , "dosulepin"
    , "doxepin"
    , "imipramine"
    , "lofepramine"
    , "nortriptyline"
    , "trimipramine"
)
cris$TCA_RELATED <- c(
    "mianserin"
    , "trazodone"
    , "nefazodone"
)
cris$ANTIDEPRESSANT_EXC_FLUPENTIXOL <- c(
    cris$SSRI
    , cris$NON_SSRI_MODERN_ANTIDEPRESSANT
    , cris$MAOI
    , cris$TCA
    , cris$TCA_RELATED
)
cris$ANTIDEPRESSANT_INC_FLUPENTIXOL <- c(
    cris$SSRI
    , cris$NON_SSRI_MODERN_ANTIDEPRESSANT
    , cris$MAOI
    , cris$TCA
    , cris$TCA_RELATED
    , "flupentixol"
)
cris$OTHER_ANTIMANIC <- c(
    "carbamazepine"
    , "valproate"
    , "lithium"
)
cris$BZ_HYPNOTIC <- c(
    "nitrazepam"
    , "flurazepam"
    , "loprazolam"
    , "lormetazepam"
    , "temazepam"
)
cris$BZ_EPILEPSY <- c(
    "clobazam"
    , "clonazepam"
)
cris$BZ_ANXIOLYTIC <- c(
    "diazepam"
    , "alprazolam"
    , "lorazepam"
    , "oxazepam"
)
cris$BZ_ANXIOLYTIC_WITHDRAWAL <- c(
    "chlordiazepoxide"
)
cris$BZ_OTHER_PERIOPERATIVE <- c(
    "midazolam"
)
cris$BZ <- c(
    cris$BZ_HYPNOTIC
    , cris$BZ_EPILEPSY
    , cris$BZ_ANXIOLYTIC
    , cris$BZ_ANXIOLYTIC_WITHDRAWAL
    , cris$BZ_OTHER_PERIOPERATIVE
)
cris$Z_DRUG <- c(
    "zaleplon"
    , "zolpidem"
    , "zopiclone"
)
cris$STIMULANT <- c(
    "amfetamine"
    , "methylphenidate"
    , "modafinil"
)
cris$ANTICHOLINERGIC <- c(
    "benzatropine"
    , "orphenadrine"
    , "procyclidine"
    , "trihexyphenidyl"
)
cris$SULFONYLUREA <- c(
    "glibenclamide"
    , "gliclazide"
    , "glimepiride"
    , "glipizide"
    , "tolbutamide"
)
cris$BIGUANIDE <- c(
    "metformin"
)
cris$ORAL_HYPOGLYCAEMIC_OTHER <- c(
    "acarbose"
    , "dapagliflozin"
    , "exenatide"
    , "linagliptin"
    , "linagliptin_with_metformin"
    , "liraglutide"
    , "lixisenatide"
    , "nateglinide"
    , "pioglitazone"
    , "pioglitazone_with_metformin"
    , "repaglinide"
    , "saxagliptin"
    , "saxagliptin_with_metformin"
    , "sitagliptin"
    , "sitagliptin_with_metformin"
    , "vildagliptin"
    , "vildagliptin_with_metformin"
)
cris$INSULIN <- c(
    "insulin" # done collectively
)
cris$STATIN <- c(
    "atorvastatin"
    , "fluvastatin"
    , "pravastatin"
    , "rosuvastatin"
    , "simvastatin"
    , "simvastatin_with_ezetimibe"
)

cris$get_any_of_several_drugs_sql <- function(druglist)
{
    ndrugs <- length(druglist)
    individualdrugs <- rep(NA, ndrugs)
    for (i in 1:ndrugs) {
        individualdrugs[i] <- cris$get_drug_sql(druglist[i])
    }
    return(
        paste(
            "( ",
            paste(individualdrugs, collapse=" OR "),
            " )",
            sep=""
        )
    )
}

cris$get_drug_sql <- function(drug, fieldname="drug")
{
    # Takes drug details as a specific drug or a drug class.
    # Returns an SQL fragment, such as "(drug = 'chlorpromazine' OR drug = 'Largactil')",
    # for a specified fieldname.
    #
    # Establish drug queries with e.g.
    #   SELECT DISTINCT drug
    #   FROM [GateDB_Camb].[dbo].[gate_medication]
    #   WHERE drug LIKE 'cloz%';
    #
    # In comments below: (*) misspelling, capitalized for brand name, (~) hybrid generic/brand name, (+) old name.

    #--------------------------------------------------------------------------
    # FIRST-GENERATION ANTIPSYCHOTICS
    #--------------------------------------------------------------------------
    if (drug == "benperidol") {
        sql <- "drug = 'benperidol' OR drug = 'Anquil'"
    }
    else if (drug == "chlorpromazine") {
        sql <- "drug = 'chlorpromazine' OR drug = 'Largactil'"
    }
    else if (drug == "flupentixol") {
        sql <- "drug LIKE 'flupent%' OR drug LIKE 'flupent%decan%' OR drug LIKE 'Depixol%' OR drug = 'Fluanxol'"
        # ... actually (CPFT 2013): flupentixol, flupentixol decanoate, flupenthixol (+), flupenthixol decanoate (+), Depixol
    }
    else if (drug == "flupentixol_depot") {
        sql <- "drug LIKE 'flupent%decan%' OR (drug LIKE 'Depixol%' AND (dose_unit = 'ml' OR route LIKE '%IM%' OR CAST(dose_value AS FLOAT) >= 20))"
        # AWKWARD: Depixol is the brand name for both a tablet and a depot version. BNF oral limit is 18 mg/day. Minimum depot unit size is 20 mg.
    }
    else if (drug == "fluphenazine") {
        # only available as a depot now?
        sql <- "drug LIKE 'fluphen%' OR drug LIKE 'Modecate%'"
        # ... actually (CPFT 2013): fluphenazine, fluphenazine decanoate, Modecate
    }
    else if (drug == "fluphenazine_depot") {
        # only available as a depot now?
        sql <- "drug LIKE 'fluphen%decan%' OR drug LIKE 'Modecate%'"
        # ... actually (CPFT 2013): fluphenazine decanoate, Modecate
    }
    else if (drug == "haloperidol") {
        sql <- "drug LIKE 'halop%' OR drug LIKE 'Dozi%' OR drug LIKE 'Hald%' OR drug LIKE 'Serena%'" # NB Serenase, Serenace
        # ... actually (CPFT 2013): haloperidol, haloperidol decanoate, Haldol, Haldol decanoate, Serenase
    }
    else if (drug == "haloperidol_depot") {
        sql <- "(drug LIKE 'halop%' AND drug LIKE '%decan%') OR (drug LIKE 'Hald%' AND drug LIKE '%decan%')"
        # ... actually (CPFT 2013): haloperidol decanoate, Haldol Decanoate
    }
    else if (drug == "levomepromazine") {
        sql <- "drug = 'levomepromazine' OR drug = 'Nozinan'"
    }
    else if (drug == "pericyazine") {
        sql <- "drug = 'pericyazine'"
    }
    else if (drug == "perphenazine") {
        sql <- "drug = 'perphenazine' OR drug = 'Fentazin' OR drug = 'Triptafen'"
        # Triptafen = amitriptyline + perphenazine
        # ... actually (CPFT 2013): perphenazine
    }
    else if (drug == "amitriptyline_with_perphenazine") { # special
        sql <- "drug = 'Triptafen'"
    }
    else if (drug == "pimozide") {
        sql <- "drug = 'pimozide' OR drug = 'Orap'"
    }
    else if (drug == "pipotiazine" || drug == "pipotiazine_depot") {
        # only available as a depot
        sql <- "drug LIKE 'pipot%' OR drug LIKE 'Piport%'"
        # ... actually (CPFT 2013): pipotiazine, Piportil
    }
    else if (drug == "prochlorperazine") {
        sql <- "drug = 'prochlorperazine' OR drug = 'Stemetil'"
    }
    else if (drug == "promazine") {
        sql <- "drug = 'promazine'"
    }
    else if (drug == "sulpiride") {
        sql <- "drug = 'sulpiride' OR drug = 'Dolmatil' OR drug = 'Sulpor'"
    }
    else if (drug == "trifluoperazine") {
        sql <- "drug = 'trifluoperazine' OR drug = 'Stelazine'"
    }
    else if (drug == "zuclopenthixol") {
        sql <- "drug LIKE 'zuclop%' OR drug LIKE 'Clopix%' OR drug = 'Acuphase'"
        # ... actually (CPFT 2013): zuclopenthixol, zuclopenthixol acetate, zuclopenthixol decanoate, Clopixol, Clopixol Decanoate, Acuphase
    }
    else if (drug == "zuclopenthixol_depot") {
        # ignores zuclopenthixol acetate (Acuphase)
        sql <- "(drug LIKE 'zuclop%' OR drug LIKE 'Clopix%') AND (drug LIKE '%decan%' OR drug LIKE '%Conc%' OR route LIKE '%IM%')"
        # ... actually (CPFT 2013): zuclopenthixol decanoate, Clopixol Decanoate
    }

    #--------------------------------------------------------------------------
    # SECOND-GENERATION ANTIPSYCHOTICS
    #--------------------------------------------------------------------------
    else if (drug == "amisulpride") {
        sql <- "drug LIKE 'amisulp%' OR drug = 'Solian'"
        # ... actually (CPFT 2013): amisulpiride(*), amisulpride, Solian
    }
    else if (drug == "aripiprazole") {
        sql <- "drug = 'aripiprazole' OR drug = 'Abilify'"
    }
    else if (drug == "asenapine") {
        sql <- "drug = 'asenapine' OR drug = 'Saphris' OR drug = 'Sycrest'"
    }
    else if (drug == "clozapine") {
        sql <- "drug LIKE 'cloz%' OR drug LIKE 'Denz%' OR drug LIKE 'Zapon%'"
        # ... actually (CPFT 2013): clozapine, Clozaril, clozepine(*)
    }
    else if (drug == "iloperidone") {
        sql <- "drug = 'iloperidone' OR drug = 'Fanapt' OR drug = 'Fanapta' OR drug = 'Zomaril'"
    }
    else if (drug == "lurasidone") {
        sql <- "drug = 'lurasidone' OR drug = 'Latuda'"
    }
    else if (drug == "olanzapine") {
        sql <- "drug LIKE 'olanz%' OR drug LIKE 'Zalast%' OR drug LIKE 'Zyprex%' OR drug LIKE 'Zypad%'"
        # ... actually (CPFT 2013): olanzapine, olanzapine embonate, olanz(*), olanzepine(*), olanzapin(*), Zyprexa
    }
    else if (drug == "olanzapine_depot") {
        sql <- "(drug LIKE 'olanz%' AND drug LIKE '%embon%') OR drug LIKE 'Zypad%'"
        # ... actually (CPFT 2013): olanzapine embonate
    }
    else if (drug == "paliperidone" || drug == "paliperidone_depot") {
        # NON-DEPOT NOW AVAILABLE; THIS QUERY DOES NOT DISCRIMINATE
        sql <- "drug = 'paliperidone' OR drug = 'Invega' OR drug = 'Xeplion'"
    }
    else if (drug == "quetiapine") {
        sql <- "drug LIKE 'quet%' OR drug = 'Seroquel'"
        # ... actually (CPFT 2013): quetiapine, quetiepine(*), Seroquel
    }
    else if (drug == "risperidone") {
        sql <- "drug LIKE 'risp%'"
        # ... actually (CPFT 2013): risperidone, risperadone(*), Risperidone Consta (~), Risperdal, Risperdal Consta
    }
    else if (drug == "risperidone_depot") {
        sql <- "drug LIKE 'risp%' AND drug LIKE '%consta'"
        # ... actually (CPFT 2013): risperidone, risperadone(*), Risperidone Consta (~), Risperdal, Risperdal Consta
    }
    else if (drug == "sertindole") {
        sql <- "drug = 'sertindole' OR drug = 'Serdolect' OR drug = 'Serlect'"
    }
    else if (drug == "ziprasidone") {
        sql <- "drug = 'ziprasidone'"
    }
    else if (drug == "zotepine") { # not in UK
        sql <- "drug = 'zotepine' OR drug = 'Nipolept' OR drug = 'Losizopilon' OR drug = 'Lodopin' OR drug = 'Setous'"
    }


    #--------------------------------------------------------------------------
    # STIMULANTS
    #--------------------------------------------------------------------------
    else if (drug == "amfetamine") {
        sql <- "drug LIKE '%amphetamine%' OR drug LIKE '%amfetamine%' OR drug = 'Adderall'"
        # ... actually (CPFT 2013): dextroamphetamine(+), dexamfetamine
    }
    else if (drug == "methylphenidate") {
        sql <- "drug = 'methylphenidate' OR drug = 'Ritalin' OR drug LIKE 'Concerta%' OR drug LIKE 'Equasym%' OR drug LIKE 'Medikinet%'"
        # ... actually (CPFT 2013): methylphenidate, Ritalin, Concerta
    }
    else if (drug == "modafinil") {
        sql <- "drug = 'modafinil' OR drug = 'Provigil'"
    }

    #--------------------------------------------------------------------------
    # ANTICHOLINERGICS
    #--------------------------------------------------------------------------
    else if (drug == "benzatropine") {
        sql <- "drug = 'benztropine' OR drug = 'benzatropine'"
    }
    else if (drug == "orphenadrine") {
        sql <- "drug = 'orphenadrine' OR drug = 'Biorphen' OR drug = 'Disipal'"
    }
    else if (drug == "procyclidine") {
        sql <- "drug = 'procyclidine' OR drug = 'Arpicolin' OR drug = 'Kemadrin'"
    }
    else if (drug == "trihexyphenidyl") {
        sql <- "drug = 'trihexyphenidyl' OR drug = 'Broflex'"
    }

    #--------------------------------------------------------------------------
    # SSRIs
    #--------------------------------------------------------------------------
    else if (drug == "citalopram") {
        sql <- "drug = 'citalopram' OR drug = 'Cipramil' OR drug = 'Celexa'"
    }
    else if (drug == "escitalopram") {
        sql <- "drug = 'escitalopram' OR drug = 'Cipralex' OR drug = 'Lexapro'"
    }
    else if (drug == "fluoxetine") {
        sql <- "drug LIKE 'fluox%' OR drug = 'Prozac' OR drug = 'Bellzac' OR drug = 'Oxactin' OR drug = 'Prozep'"
        # ... actually (CPFT 2013): fluoxetine, "fluoxetine  Dec" (??)
    }
    else if (drug == "fluvoxamine") { # maleate
        sql <- "drug = 'fluvoxamine' OR drug = 'Luvox' OR drug = 'Faverin'"
    }
    else if (drug == "paroxetine") {
        sql <- "drug = 'paroxetine' OR drug = 'Seroxat' OR drug = 'Paxil'"
        # there are other brands elsewhere...
    }
    else if (drug == "sertraline") {
        sql <- "drug = 'sertraline' OR drug = 'Lustral' OR drug = 'Zoloft' OR drug = 'Bellsert'"
        # NOT Seretra (cf. SLAM code, see email to self 2016-12-02); Seretra = seratrodast = for asthma
    }

    #--------------------------------------------------------------------------
    # OTHER MODERN ANTIDEPRESSANTS
    #--------------------------------------------------------------------------
    else if (drug == "agomelatine") {
        sql <- "drug = 'agomelatine' OR drug = 'Valdoxan'"
    }
    else if (drug == "bupropion") { # antidepressant license in US, smoking cessation in UK
        sql <- "drug = 'bupropion' OR drug = 'Zyban'"
    }
    else if (drug == "duloxetine") {
        sql <- "drug = 'duloxetine' OR drug = 'Cymbalta' OR drug = 'Yentreve'"
    }
    else if (drug == "mirtazapine") {
        sql <- "drug LIKE 'mirtaz%' OR drug = 'mirtazepine' OR drug = 'Zispin' OR drug = 'Mirza'"
        # ... actually (CPFT 2013): mirtazapine, mirtazepine(*), "mirtazapine Dec" (?)
    }
    else if (drug == "reboxetine") {
        sql <- "drug = 'reboxetine' OR drug = 'Edronax'"
    }
    else if (drug == "tryptophan") {
        sql <- "drug = 'tryptophan' OR drug = 'Optimax'"
    }
    else if (drug == "venlafaxine") {
        sql <- "drug LIKE 'venla%' OR drug LIKE 'Efexor%' OR drug LIKE 'Effexor%'"
        # ... actually (CPFT 2013): venlafaxine, venlafaxine XL,
    }

    #--------------------------------------------------------------------------
    # TRICYCLIC AND RELATED ANTIDEPRESSANTS
    #--------------------------------------------------------------------------
    else if (drug == "amitriptyline") {
        sql <- "drug LIKE 'amitr_pt_l_n%' OR drug = 'Vanatrip' OR drug = 'Elavil' OR drug = 'Endep' OR drug = 'Triptafen'"
        # ... actually (CPFT 2013): amitriptyline, amitriptiline(*), amitryptyline(*)
        # Triptafen = amitriptyline + perphenazine
    }
    # see also amitriptyline_with_perphenazine, above
    else if (drug == "clomipramine") {
        sql <- "drug = 'clomipramine' OR drug LIKE 'Anafranil%'"
    }
    else if (drug == "dosulepin") {
        sql <- "drug = 'dosulepin' OR drug = 'dothiepin' OR drug = 'Prothiaden'"
        # ... actually (CPFT 2013): dosulepin, dothiepin(+)
    }
    else if (drug == "doxepin") {
        sql <- "drug = 'doxepin' OR drug = 'Sinepin' OR drug = 'Sinequan' OR drug = 'Sinepin' OR drug = 'Xepin'"
        # Xepin is cream only
    }
    else if (drug == "imipramine") {
        sql <- "drug = 'imipramine' OR drug = 'Tofranil'"
    }
    else if (drug == "lofepramine") {
        sql <- "drug = 'lofepramine' OR drug = 'Lomont'"
    }
    else if (drug == "nortriptyline") {
        sql <- "drug LIKE 'nortr%' OR drug = 'Allegron' OR drug = 'Pamelor' OR drug = 'Aventyl'"
        # ... actually (CPFT 2013): nortriptyline, nortryptiline(*)
    }
    else if (drug == "trimipramine") {
        sql <- "drug = 'trimipramine' OR drug = 'Surmontil'"
    }

    #--------------------------------------------------------------------------
    # TRICYCLIC-RELATED ANTIDEPRESSANTS
    #--------------------------------------------------------------------------
    else if (drug == "mianserin") {
        sql <- "drug = 'mianserin'"
    }
    else if (drug == "trazodone") {
        sql <- "drug = 'trazodone' OR drug = 'Molipaxin'"
    }
    else if (drug == "nefazodone") {
        # discontinued for hepatotoxicity? But apparently still used in 2014
        # in the UK: http://www.bbc.co.uk/news/uk-25745824
        sql <- "drug = 'nefazodone' OR drug = 'Dutonin' OR drug = 'Nefadar' OR drug = 'Serzone'"
        # brand names from https://en.wikipedia.org/wiki/Nefazodone
        # ... yup, still a trickle, mostly from Islington:
        # https://openprescribing.net/chemical/0403040T0/
    }

    #--------------------------------------------------------------------------
    # MAOIs
    #--------------------------------------------------------------------------
    else if (drug == "phenelzine") {
        sql <- "drug = 'phenelzine' OR drug = 'Nardil'"
        # SLAM code (see e-mail to self 2016-12-02) also has %Alazin%; not sure that's right
        # se also http://www.druglib.com/activeingredient/phenelzine/
    }
    # not included: pheniprazine
    else if (drug == "isocarboxazid") {
        sql <- "drug = 'isocarboxazid'"
    }
    else if (drug == "moclobemide") {
        sql <- "drug = 'moclobemide'"
    }
    else if (drug == "tranylcypromine") {
        sql <- "drug = 'tranylcypromine' OR drug = 'Parnate'"
    }

    #--------------------------------------------------------------------------
    # BENZODIAZEPINES
    #--------------------------------------------------------------------------
    else if (drug == "alprazolam") {
        sql <- "drug = 'alprazolam'"
    }
    else if (drug == "chlordiazepoxide") {
        sql <- "drug = 'chlordiazepoxide'"
    }
    else if (drug == "clobazam") {
        sql <- "drug = 'clobazam'"
    }
    else if (drug == "clonazepam") {
        sql <- "drug = 'clonazepam' OR drug = 'Rivotril'"
    }
    else if (drug == "diazepam") {
        sql <- "drug LIKE 'diaz%' OR drug = 'Valium'"
        # ... actually (CPFT 2013): diazepam, diazapam(*), diazapem(*), Valium
    }
    else if (drug == "flurazepam") {
        sql <- "drug = 'flurazepam' OR drug = 'Dalmane'"
    }
    else if (drug == "loprazolam") {
        sql <- "drug = 'loprazolam'"
    }
    else if (drug == "lorazepam") {
        sql <- "drug = 'lorazepam' OR drug = 'Ativan'"
    }
    else if (drug == "lormetazepam") {
        sql <- "drug = 'lormetazepam'"
    }
    else if (drug == "midazolam") {
        sql <- "drug = 'midazolam' OR drug = 'Hypnovel'"
    }
    else if (drug == "nitrazepam") {
        sql <- "drug = 'nitrazepam'"
    }
    else if (drug == "oxazepam") {
        sql <- "drug = 'oxazepam'"
    }
    else if (drug == "temazepam") {
        sql <- "drug = 'temazepam'"
    }

    #--------------------------------------------------------------------------
    # Z-DRUGS
    #--------------------------------------------------------------------------
    else if (drug == "zaleplon") {
        sql <- "drug = 'zaleplon' OR drug = 'Sonata'"
    }
    else if (drug == "zolpidem") { # (tartrate)
        sql <- "drug LIKE 'zolpidem%' OR drug = 'Stilnoct'"
        # ... actually (CPFT 2013): zolpidem, zolpidem tartrate
    }
    else if (drug == "zopiclone") {
        sql <- "drug = 'zopiclone' OR drug = 'Zimovane'"
    }

    #--------------------------------------------------------------------------
    # OTHER ANXIOLYTICS
    #--------------------------------------------------------------------------
    else if (drug == "buspirone") {
        sql <- "drug = 'buspirone' OR drug = 'Buspar'"
    }

    #--------------------------------------------------------------------------
    # OTHER ANTIMANIC
    #--------------------------------------------------------------------------
    else if (drug == "carbamazepine") {
        sql <- "drug = 'carbamazepine' OR drug LIKE 'Carbagen%' OR drug LIKE 'Tegretol%'" # also Tegretol Prolonged Release (formerly Tegretol Retard)
        # ... actually (CPFT 2013): carbamazepine, Tegretol
    }
    else if (drug == "valproate") {
        sql <- "drug LIKE '%valp%' OR drug LIKE 'Epilim%' OR drug = 'Episenta' OR drug = 'Epival' OR drug = 'Convulex' OR drug = 'Depakote'"
        # ... also semisodium valproate
        # ... actually (CPFT 2013): sodium valproate [chrono], valproic acid, valproate, sodium valproate, sodium valporate(*), sodium valporate(*) chrono, Depakote
    }
    else if (drug == "lithium") {
        sql <- "drug LIKE 'lithium%' OR drug = 'Camcolit' OR drug = 'Liskonum' OR drug = 'Priadel' OR drug = 'Li-Liquid'"
        # ... actually (CPFT 2013): lithium, lithium carbonate, lithium citrate (curious: Priadel must be being changed to lithium...)
    }

    #--------------------------------------------------------------------------
    # Other for bipolar/unipolar depression
    #--------------------------------------------------------------------------
    else if (drug == "lamotrigine") {
        sql <- "drug LIKE 'lamotrigine%' OR drug = 'Lamictal'"
    }
    else if (drug == "triiodothyronine") {
        sql <- "drug = 'triiodothyronine' OR drug = 'tri-iodothyronine' OR drug = 'liothyronine' OR drug='Cytomel'"
    }

    #--------------------------------------------------------------------------
    # GENERAL MEDICINE: DIABETES
    #--------------------------------------------------------------------------
    # Sulfonylureas
    else if (drug == "glibenclamide") {
        sql <- "drug = 'glibenclamide'"
    }
    else if (drug == "gliclazide") {
        sql <- "drug = 'gliclazide' OR drug = 'Zicron' OR drug LIKE 'Diamicron%' OR drug LIKE 'Dacadis%' OR drug LIKE 'Vitile%'"
    }
    else if (drug == "glimepiride") {
        sql <- "drug = 'glimepiride' OR drug = 'Amaryl'"
    }
    else if (drug == "glipizide") {
        sql <- "drug = 'glipizide' OR drug = 'Minodiab'"
    }
    else if (drug == "tolbutamide") {
        sql <- "drug = 'tolbutamide'"
    }

    # Biguanides (see also combinations below)
    else if (drug == "metformin") {
        sql <- "drug LIKE 'metformin%' OR drug LIKE 'Glucophage%'"
    }

    # Other
    else if (drug == "acarbose") {
        sql <- "drug = 'acarbose' OR drug = 'Glucobay'"
    }
    else if (drug == "dapagliflozin") {
        sql <- "drug = 'dapagliflozin' OR drug = 'Forxiga'"
    }
    else if (drug == "exenatide") {
        sql <- "drug = 'exenatide' OR drug = 'Byetta' OR drug = 'Bydureon'"
    }
    else if (drug == "linagliptin") {
        sql <- "drug = 'linagliptin' OR drug = 'Trajenta'"
    }
    else if (drug == "linagliptin_with_metformin") {
        sql <- "drug = 'Jentadueto'"
    }
    else if (drug == "liraglutide") {
        sql <- "drug = 'liraglutide' OR drug = 'Victoza'"
    }
    else if (drug == "lixisenatide") {
        sql <- "drug = 'lixisenatide' OR drug = 'Lyxumia'"
    }
    else if (drug == "nateglinide") {
        sql <- "drug = 'nateglinide' OR drug = 'Starlix'"
    }
    else if (drug == "nateglinide") {
        sql <- "drug = 'nateglinide' OR drug = 'Starlix'"
    }
    else if (drug == "pioglitazone") {
        sql <- "drug = 'pioglitazone' OR drug = 'Actos'"
    }
    else if (drug == "pioglitazone_with_metformin") {
        sql <- "drug = 'Competact'"
    }
    else if (drug == "repaglinide") {
        sql <- "drug = 'repaglinide' OR drug = 'Prandin'"
    }
    else if (drug == "saxagliptin") {
        sql <- "drug = 'saxagliptin' OR drug = 'Onglyza'"
    }
    else if (drug == "saxagliptin_with_metformin") {
        sql <- "drug = 'Komboglyze'"
    }
    else if (drug == "sitagliptin") {
        sql <- "drug = 'sitagliptin' OR drug = 'Januvia'"
    }
    else if (drug == "sitagliptin_with_metformin") {
        sql <- "drug = 'Janumet'"
    }
    else if (drug == "vildagliptin") {
        sql <- "drug = 'vildagliptin' OR drug = 'Galvus'"
    }
    else if (drug == "vildagliptin_with_metformin") {
        sql <- "drug = 'Eucreas'"
    }

    # Insulin. Covering the BNF categories:
    # INSULIN
    # INSULIN ASPART
    # INSULIN GLULISINE
    # INSULIN LISPRO
    # INSULIN DEGLUDEC
    # INSULIN DETEMIR
    # INSULIN GLARGINE
    # INSULIN ZINC SUSPENSION
    # ISOPHANE INSULIN
    # PROTAMINE ZINC INSULIN
    # BIPHASIC INSULIN ASPART
    # BIPHASIC INSULIN LISPRO
    # BIPHASIC ISOPHANE INSULIN
    else if (drug == "insulin") {  # bugfix 2018-02-19: was "metformin"! Didn't affect any actual data thus far.
        sql <- paste(
            "drug LIKE '%insulin%'"
            , "drug LIKE '%aspart%'"
            , "drug LIKE '%glulisine%'"
            , "drug LIKE '%lispro%'"
            , "drug LIKE '%degludec%'"
            , "drug LIKE '%detemir%'"
            , "drug LIKE '%glargine%'"

            , "drug LIKE '%Hypurin%'"
            , "drug LIKE '%Actrapid%'"
            , "drug LIKE '%Humulin%'"
            , "drug LIKE '%Insuman%'"
            , "drug LIKE '%Novorapid%'"
            , "drug LIKE '%Apidra%'"
            , "drug LIKE '%Humalog%'"
            , "drug LIKE '%Tresiba%'"
            , "drug LIKE '%Levemir%'"
            , "drug LIKE '%Lantus%'"
            , "drug LIKE '%Insulatard%'"
            , "drug LIKE '%NovoMix%'"
            , sep=" OR "
        )
    }

    #--------------------------------------------------------------------------
    # GENERAL MEDICINE: CARDIOVASCULAR
    #--------------------------------------------------------------------------
    else if (drug == "aspirin") {
        sql <- "drug = 'aspirin'"
        # ... actually (CPFT 2013): aspirin
    }
    else if (drug == "atenolol") {
        sql <- "drug = 'atenolol'"
    }
    # ACE inhibitors (selected)
    else if (drug == "lisinopril") {
        sql <- "drug = 'lisinopril'"
    }
    else if (drug == "ramipril") {
        sql <- "drug = 'ramipril'"
    }
    # Statins
    else if (drug == "atorvastatin") {
        sql <- "drug = 'atorvastatin' OR drug = 'Lipitor'"
    }
    else if (drug == "fluvastatin") {
        sql <- "drug = 'fluvastatin' OR drug LIKE 'Lescol%'"
    }
    else if (drug == "pravastatin") {
        sql <- "drug = 'pravastatin' OR drug = 'Lipostat'"
    }
    else if (drug == "rosuvastatin") {
        sql <- "drug = 'rosuvastatin' OR drug = 'Crestor'"
    }
    else if (drug == "simvastatin") {
        sql <- "drug = 'simvastatin' OR drug = 'Zocor'"
    }
    else if (drug == "simvastatin_with_ezetimibe") {
        sql <- "drug = 'Inegy'"
    }

    #--------------------------------------------------------------------------
    # GENERAL MEDICINE: RESPIRATORY
    #--------------------------------------------------------------------------
    else if (drug == "salbutamol") {
        sql <- "drug LIKE 'salbut%' OR drug like 'vent%'"
        # ... actually (CPFT 2013): salbutamol
    }

    #--------------------------------------------------------------------------
    # GENERAL MEDICINE: GASTROINTESTINAL
    #--------------------------------------------------------------------------
    else if (drug == "lactulose") {
        sql <- "drug LIKE 'lactul%' OR drug LIKE 'Duphal%' OR drug LIKE 'Lactug%' OR drug LIKE 'laevol%'"
        # ... actually (CPFT 2013): lactulose
    }
    else if (drug == "lansoprazole") {
        sql <- "drug = 'lansoprazole'"
    }
    else if (drug == "omeprazole") {
        sql <- "drug = 'omeprazole'"
    }
    else if (drug == "senna") {
        sql <- "drug = 'senna'"
    }

    #--------------------------------------------------------------------------
    # GENERAL MEDICINE: OTHER
    #--------------------------------------------------------------------------
    else if (drug == "ibuprofen") {
        sql <- "drug = 'ibuprofen'"
    }
    else if (drug == "levothyroxine") {
        sql <- "drug LIKE '%thyroxine%'"
        # ... actually (CPFT 2013): levothyroxine, thyroxine
    }
    else if (drug == "paracetamol") {
        sql <- "drug = 'paracetamol'"
        # ... actually (CPFT 2013): salbutamol
    }
    else if (drug == "thiamine") {
        sql <- "drug = 'thiamine'"
    }

    #--------------------------------------------------------------------------
    # CATEGORIES, NOT INDIVIDUAL DRUGS -- NB partly recursive, so beware self-inclusion!
    #
    # Note intended use: e.g.
    #   - use everything in cris$SSRI, separately...
    #   - or use "ssri" here to get them collectively.
    #--------------------------------------------------------------------------
    else if (drug == "antipsychotic") {
        sql <- cris$get_any_of_several_drugs_sql(c(
            cris$FGA
            , cris$SGA
        ))
    }
    else if (drug == "antipsychotic_depot") {
        sql <- cris$get_any_of_several_drugs_sql(c(
            cris$FGA_DEPOT
            , cris$SGA_DEPOT
        ))
    }
    else if (drug == "fga") {
        sql <- cris$get_any_of_several_drugs_sql(cris$FGA)
    }
    else if (drug == "fga_depot") {
        sql <- cris$get_any_of_several_drugs_sql(cris$FGA_DEPOT)
    }
    else if (drug == "sga") {
        sql <- cris$get_any_of_several_drugs_sql(cris$SGA)
    }
    else if (drug == "sga_not_clozapine") {
        sql <- cris$get_any_of_several_drugs_sql(cris$SGA_NOT_CLOZAPINE)
    }
    else if (drug == "sga_depot") {
        sql <- cris$get_any_of_several_drugs_sql(cris$SGA_DEPOT)
    }
    else if (drug == "ssri") {
        sql <- cris$get_any_of_several_drugs_sql(cris$SSRI)
    }
    else if (drug == "non_ssri_modern_antidepressant") {
        sql <- cris$get_any_of_several_drugs_sql(cris$NON_SSRI_MODERN_ANTIDEPRESSANT)
    }
    else if (drug == "maoi") {
        sql <- cris$get_any_of_several_drugs_sql(cris$MAOI)
    }
    else if (drug == "tca") {
        sql <- cris$get_any_of_several_drugs_sql(cris$TCA)
    }
    else if (drug == "tca_related") {
        sql <- cris$get_any_of_several_drugs_sql(cris$TCA_RELATED)
    }
    else if (drug == "bz") {
        sql <- cris$get_any_of_several_drugs_sql(cris$BZ)
    }
    else if (drug == "z_drug") {
        sql <- cris$get_any_of_several_drugs_sql(cris$Z_DRUG)
    }
    else if (drug == "stimulant") {
        sql <- cris$get_any_of_several_drugs_sql(cris$STIMULANT)
    }
    else if (drug == "anticholinergic") {
        sql <- cris$get_any_of_several_drugs_sql(cris$ANTICHOLINERGIC)
    }

    else if (drug == "sulfonylurea") {
        sql <- cris$get_any_of_several_drugs_sql(cris$SULFONYLUREA)
    }
    else if (drug == "biguanide") {
        sql <- cris$get_any_of_several_drugs_sql(cris$BIGUANIDE)
    }
    else if (drug == "oral_hypoglycaemic_other") {
        sql <- cris$get_any_of_several_drugs_sql(cris$ORAL_HYPOGLYCAEMIC_OTHER)
    }
    else if (drug == "diabetes_mellitus") {
        sql <- cris$get_any_of_several_drugs_sql(
            c(
                cris$SULFONYLUREA
                , cris$BIGUANIDE
                , cris$ORAL_HYPOGLYCAEMIC_OTHER
                , cris$INSULIN
            )
        )
    }
    else if (drug == "statin") {
        sql <- cris$get_any_of_several_drugs_sql(cris$STATIN)
    }

    #--------------------------------------------------------------------------
    # NOT FOUND...
    #--------------------------------------------------------------------------
    else {
        stop(paste("INVALID DRUG SPECIFIED:", drug))
    }

    return(paste("(",
                 gsub("drug ", paste(fieldname, " ", sep=""), sql),
                 ")", sep=""))
}

cris$map_drugs_to_uniform_names_sql <- function(fieldname, druglist)
{
    sql <- paste("
            CASE
        ",
        sep=""
    )
    for (drug in druglist) {
        sql <- paste(sql, "
                WHEN ", cris$get_drug_sql(drug, fieldname), " THEN '", drug, "'
            ",
            sep=""
        )
    }
    sql <- paste(sql, "
                ELSE NULL
            END
        ",
        sep=""
    )
    return(sql)
}

#==============================================================================
# Diagnoses by ICD-10 code
#==============================================================================

cris$get_icd10code_sql <- function(condition, fieldname="code")
{
    # Returns an SQL fragment, such as "(code LIKE 'F20%')"
    if (condition == "schizophrenia") {
        sql <- "code LIKE 'F20%'"
    }
    else if (condition == "recurrent_depressive_disorder") {
        sql <- "code LIKE 'F33%'"
    }
    else if (condition == "depressive_episode_or_recurrent_depressive_disorder") {
        sql <- "code LIKE 'F32%' OR code LIKE 'F33%'"
    }
    else if (condition == "manic_hypomanic_episode_or_bipolar_affective_disorder") {
        sql <- "code LIKE 'F30%' OR code LIKE 'F31%'"
    }
    #--------------------------------------------------------------------------
    # NOT FOUND...
    #--------------------------------------------------------------------------
    else {
        stop(paste("INVALID CONDITION SPECIFIED:", condition))
    }

    return(paste("(",
                 gsub("code ", paste(fieldname, " ", sep=""), sql),
                 ")", sep=""))
}

cris$cpft.most_popular_icd10_diagnoses <- function(dbhandle)
{
    sqlQuery(dbhandle, "
        SELECT code, description, COUNT(*) AS NumInstancesOfDiagnosis
        FROM cpft_endsql.dbo.diagnosis
        GROUP BY code, description
        ORDER BY COUNT(*) DESC
    ")
}

cris$cpft.n_patients <- function(dbhandle)
{
    r <- sqlQuery(dbhandle, "
        SELECT COUNT(DISTINCT brcid)
        FROM cpft_endsql.dbo.mpi
    ")
    return(r[1,1])
}

cris$cpft.n_patients_with_any_diagnosis <- function(dbhandle)
{
    r <- sqlQuery(dbhandle, "
        SELECT COUNT(DISTINCT brcid)
        FROM cpft_endsql.dbo.diagnosis
    ")
    return(r[1,1])
}

cris$cpft.n_patients_with_any_diagnosis_from_list <- function(dbhandle, idlist)
{
    # Beware with very long lists - will crash the SQL engine
    r <- sqlQuery(dbhandle, paste("
        SELECT COUNT(DISTINCT brcid)
        FROM cpft_endsql.dbo.diagnosis
        WHERE ", cris$sql_id_in_list(idlist, "BrcId"), "
    ", sep=""))
    return(r[1,1])
}

cris$cpft.n_patients_with_any_diagnosis_alive_on_or_after <- function(
    dbhandle,
    startdate)
{
    start_date_as_text <- as.character(startdate)
    r <- sqlQuery(dbhandle, paste("
        SELECT COUNT(DISTINCT brcid)
        FROM cpft_endsql.dbo.diagnosis D
        WHERE D.brcid IN (
            SELECT brcid
            FROM cpft_endsql.dbo.mpi M
            WHERE
                M.truncated_dttm = 0 /* not dead */
                OR CAST(M.truncated_dttm AS DATE) >= '", start_date_as_text, "' /* alive at the start */
        )
    ", sep=""))
    return(r[1,1])
}

cris$cpft.n_patients_alive_on_or_after <- function(dbhandle, startdate)
{
    start_date_as_text <- as.character(startdate)
    r <- sqlQuery(dbhandle, paste("
        SELECT COUNT(*)
        FROM cpft_endsql.dbo.mpi M
        WHERE
            M.truncated_dttm = 0 /* not dead */
            OR CAST(M.truncated_dttm AS DATE) >= '", start_date_as_text, "' /* alive at the start */
    ", sep=""))
    return(r[1,1])
}

cris$cpft.diagnosis_frequency_for_specified_patients <- function(dbhandle,
                                                                 idlist)
{
    # Beware with very long lists - will crash the SQL engine
    r <- sqlQuery(dbhandle, paste("
        SELECT code, description, COUNT(DISTINCT brcid) AS n_patients
        FROM cpft_endsql.dbo.diagnosis
        WHERE ", cris$sql_id_in_list(idlist, "BrcId"), "
        GROUP BY code, description
        ORDER BY COUNT(DISTINCT brcid) DESC
    ", sep=""))
    return(r)
}

#==============================================================================
# SQL manipulation
#==============================================================================

cris$sql_id_in_list <- function(idlist, fieldname)
{
    paste(
        fieldname
        , " IN ("
        , paste(idlist, collapse=", ")
        , ")"
        , sep=""
    )
}

cris$sql_id_not_in_list <- function(idlist, fieldname)
{
    paste(
        fieldname
        , " NOT IN ("
        , paste(idlist, collapse=", ")
        , ")"
        , sep=""
    )
}

#==============================================================================
# CPFT queries
#
# always prefer dates, not datetimes, when possible (or get off-by-one-hour problems)
#
#==============================================================================

cris$cpft.approx_total_admission_days_per_year_all_patients <- function(dbhandle)
{
    sqlQuery(dbhandle, "
        SELECT
            DATEPART(YEAR, W.start_dttm) AS Year
            , COUNT(*) AS NumAdmissions
            , SUM(DATEDIFF(dd, W.start_dttm, W.end_dttm)) AS TotalAdmissionDays
        FROM cpft_endsql.dbo.Ward_Stays W
        WHERE
            W.start_dttm <> 0 /* eliminate quasi-NULL values */
            AND W.end_dttm <> 0 /* eliminate quasi-NULL values */
        GROUP BY
            DATEPART(YEAR, W.start_dttm)
        ORDER BY
            DATEPART(YEAR, W.start_dttm)
    ")
}

cris$cpft.total_documents_per_year_all_patients <- function(dbhandle)
{
    sqlQuery(dbhandle, "
        SELECT
            DATEPART(YEAR, L.EventTime) AS Year,
            COUNT(*) AS NumDocuments
        FROM cpft_endsql.dbo.DocumentLibrary L
        WHERE
            L.EventTime <> 0 /* eliminate quasi-NULL values */
        GROUP BY
            DATEPART(YEAR, L.EventTime)
        ORDER BY
            DATEPART(YEAR, L.EventTime)
    ")
}

cris$cpft.get_patients <- function(
    dbhandle,
    startdate,
    enddate,
    diagnosis=NULL,
    exclude_id_list=NULL)
{
    if (is.null(diagnosis) && is.null(exclude_id_list)){
        stop("specify diagnosis or exclude_id_list")
    }
    if (!is.null(diagnosis) && !is.null(exclude_id_list)){
        stop("don't specify diagnosis and exclude_id_list")
    }
    start_date_as_text <- as.character(startdate)
    end_date_as_text <- as.character(enddate)
    query = paste("
        SELECT
            M.BrcId AS id
            , M.gender AS sex
            , CAST(M.cleaneddateofbirth AS DATE) AS dob
            , CASE
                WHEN M.truncated_dttm = 0 THEN NULL /* not dead */
                ELSE CAST(M.truncated_dttm AS DATE) /* dead */
            END AS dod
            , CASE
                WHEN M.truncated_dttm = 0 THEN CAST('", end_date_as_text, "' AS DATE) /* not dead */ /* not CURRENT_TIMESTAMP as CRIS export not nightly */
                    /* not CURRENT_TIMESTAMP as CRIS export not nightly */
                ELSE CAST(M.truncated_dttm AS DATE) /* dead */
            END AS final_date
            , (
                /* Date of first letter after search start date */
                SELECT CAST(MIN(EventTime) AS DATE)
                FROM cpft_endsql.dbo.DocumentLibrary L
                WHERE L.BrcId = M.BrcId
                AND L.EventTime <> 0 /* eliminate quasi-NULL values */
                AND L.EventTime >= '", start_date_as_text, "'
                AND L.EventTime <= '", end_date_as_text, "'
            ) AS first_letter_date_in_time_range
            , (
                /* Date of last letter after search start date */
                SELECT CAST(MAX(EventTime) AS DATE)
                FROM cpft_endsql.dbo.DocumentLibrary L
                WHERE L.BrcId = M.BrcId
                AND L.EventTime <> 0 /* eliminate quasi-NULL values */
                AND L.EventTime >= '", start_date_as_text, "'
                AND L.EventTime <= '", end_date_as_text, "'
            ) AS last_letter_date_in_time_range
            , (
                /* Start of first admission after search start date */
                SELECT CAST(MIN(start_dttm) AS DATE)
                FROM cpft_endsql.dbo.Ward_Stays W
                WHERE W.BrcId = M.BrcId
                AND W.start_dttm <> 0 /* eliminate quasi-NULL values */
                AND W.end_dttm <> 0 /* eliminate quasi-NULL values */
                AND W.start_dttm >= '", start_date_as_text, "'
                AND W.start_dttm <= '", end_date_as_text, "'
            ) AS first_admission_date_in_time_range
            , (
                /* End of last admission beginning after search start date and finishing before/on last start date */
                SELECT CAST(MAX(end_dttm) AS DATE)
                FROM cpft_endsql.dbo.Ward_Stays W
                WHERE W.BrcId = M.BrcId
                AND W.start_dttm <> 0 /* eliminate quasi-NULL values */
                AND W.end_dttm <> 0 /* eliminate quasi-NULL values */
                AND W.start_dttm >= '", start_date_as_text, "'
                AND W.end_dttm <= '", end_date_as_text, "'
            ) AS end_of_last_completed_admission_in_time_range
    ", sep="")
    if (!is.null(diagnosis)) {
        icd10where <- cris$get_icd10code_sql(diagnosis, fieldname="D.CODE")
        query <- paste(query, "
            , (
                /* Date of first ICD-10 diagnosis matching diagnosis criterion (regardless of time) */
                SELECT CAST(MIN(dgpro_dttm) AS DATE)
                FROM cpft_endsql.dbo.Diagnosis D
                WHERE D.BrcId = M.BrcId
                AND D.dgpro_dttm <> 0 /* eliminate quasi-NULL values */
                AND (", icd10where, ")
            ) AS first_diagnosis_date
        ", sep="")
    }
    query <- paste(query, "
        FROM
            cpft_endsql.dbo.MPI M
        WHERE
    ", sep="")
    if (!is.null(diagnosis)) {
        query <- paste(query, "
            /* Our patients should have the specified diagnosis -- at any time in the clinical record */
            EXISTS (
                SELECT *
                FROM cpft_endsql.dbo.Diagnosis D
                WHERE D.BrcId = M.BrcId
                AND (", icd10where, ")
            )
        ", sep="")
    } else {
        query <- paste(query, "
            /* the ID is NOT in a specified list */
            ", cris$sql_id_not_in_list(exclude_id_list, "M.BrcId"), "
        ", sep="")
    }
    query <- paste(query, "
            /* We don't want people who died before our period of interest: */
            AND (
                M.truncated_dttm = 0 /* not dead */
                OR CAST(M.truncated_dttm AS DATE) >= '", start_date_as_text, "'
                    /* alive at the start */
            )
        ORDER BY brcid
    ", sep="")

    x <- sqlQuery(dbhandle, query)

    within(x, {
        dob <- as.Date(dob)
        dod <- as.Date(dod)
        final_date <- as.Date(final_date)
        first_letter_date_in_time_range <- as.Date(first_letter_date_in_time_range)
        last_letter_date_in_time_range <- as.Date(last_letter_date_in_time_range)
        first_admission_date_in_time_range <- as.Date(first_admission_date_in_time_range)
        end_of_last_completed_admission_in_time_range <- as.Date(end_of_last_completed_admission_in_time_range)

        died <- !is.na(dod)
        age_at_death <- cris$years_between(dob, dod)
        age_at_end <- cris$years_between(dob, final_date)

        if (!is.null(diagnosis)) {
            first_diagnosis_date <- as.Date(first_diagnosis_date)
            age_at_first_diagnosis <- cris$years_between(dob, first_diagnosis_date)
        } else {
            first_diagnosis_date <- as.Date(NA) # COERCED IN
            age_at_first_diagnosis <- as.numeric(NA) # COERCED IN # cris$years_between(dob, first_diagnosis_date)
        }
    })
}

cris$cpft.duration_of_follow_up <- function(patientdf) {
    cris$years_between(
        pmin(patientdf$first_letter_date_in_time_range,
             patientdf$first_admission_date_in_time_range,
             na.rm=TRUE),
        pmax(patientdf$last_letter_date_in_time_range,
             patientdf$end_of_last_completed_admission_in_time_range,
             na.rm=TRUE)
    )
}

cris$cpft.all_admissions_between <- function(dbhandle, idlist,
                                             startdate, enddate)
{
    # NOTE: may produce admissions starting well before the apparent start date,
    # which is fine (e.g. admitted continuously 1998-2010).
    start_date_as_text <- as.character(startdate)
    end_date_as_text <- as.character(enddate)
    x <- sqlQuery(dbhandle, paste("
        SELECT
            brcid AS id
            , DATEPART(YEAR, W.start_dttm) AS year
            , CAST(start_dttm AS DATE) AS admission_date
            , CAST(end_dttm AS DATE) AS discharge_date
            , DATEDIFF(dd, start_dttm, end_dttm) AS duration_days
        FROM cpft_endsql.dbo.Ward_Stays W
        WHERE
            W.start_dttm <> 0 /* eliminate quasi-NULL values */
            AND W.end_dttm <> 0 /* eliminate quasi-NULL values */
            AND W.end_dttm >= '", start_date_as_text, "'
            AND W.start_dttm <= '", end_date_as_text, "'
            AND ", cris$sql_id_in_list(idlist, "W.BrcId"), "
        ORDER BY
            brcid,
            start_dttm
    ", sep=""))

    within(x, {
        admission_date <- as.Date(admission_date)
        discharge_date <- as.Date(discharge_date)
    })
}

cris$cpft.admissions_between_inclusive <- function(admissions_df, patient_id,
                                                   startdate, enddate)
{
    # ASSUMES NO OVERLAPS
    # An admission from 1-2 Jan counts as 1 day, not 2
    # Do not use "id" as the function argument -- treat as a reserved word
    # (esp. in the next statement)
    # Passing patient_id=NA means calculate the average for all patients
    # represented
    if (is.na(patient_id)) {
        admission_date <- admissions_df$admission_date
        discharge_date <- admissions_df$discharge_date
        n_patients <- length(unique(admissions_df$id))
    }
    else {
        admission_date <- admissions_df$admission_date[admissions_df$id == patient_id]
        discharge_date <- admissions_df$discharge_date[admissions_df$id == patient_id]
        n_patients <- 1
    }

    count_admission_start <- ifelse(startdate <= admission_date & admission_date <= enddate, 1, 0)

    count_inpatient_days <- ifelse(
        enddate < admission_date,
        0, # admission too late to count?
        ifelse(
            discharge_date < startdate,
            0, # admission too early to count
            ifelse(
                admission_date < startdate,
                ifelse(
                    enddate < discharge_date,
                    as.integer(enddate - startdate),
                    # ... admission,START,___,END,discharge
                    as.integer(discharge_date - startdate)
                    # ... admission,START,___,discharge,END
                ),
                ifelse(
                    enddate < discharge_date,
                    as.integer(enddate - admission_date),
                    # ... START,admission,___,END,discharge
                    as.integer(discharge_date - admission_date)
                    # ... START,admission,___,discharge,END
                )
            )
        )
    )

    c(
        "admission_days"=sum(count_inpatient_days)/n_patients,
        "admissions"=sum(count_admission_start)/n_patients
    )
}

cris$cpft.admissions_as_binary <- function(admissions_df, patient_id, alldates)
{
    # alldates must be a sorted vector of dates
    n <- length(alldates)
    admitted <- numeric(n) # zeros
    first_date <- alldates[1]
    last_date <- alldates[n]
    admission_dates <- admissions_df$admission_date[admissions_df$id == patient_id]
    discharge_dates <- admissions_df$discharge_date[admissions_df$id == patient_id]
    for (i in seq_along(admission_dates)) {
        adm <- admission_dates[i]
        dis <- discharge_dates[i]
        if (adm > last_date) next
        if (dis < first_date) next
        start_index <- ifelse(adm < first_date,
                              1,
                              which(alldates == admission_dates[i]))
        end_index   <- ifelse(dis > last_date,
                              n,
                              which(alldates == discharge_dates[i]))
        admitted[start_index : end_index] <- 1
    }
    return(admitted)
}

cris$cpft.admissions_by_patient_year <- function(
    admissions_df
    , patient_df
    , years
    , EXCLUDE_YEARS_NOT_WITHIN_EACH_PATIENT_DATE_RANGE=TRUE)
{
    years_df <- data.frame(
        year=years
    )
    d <- sqldf("
        SELECT
            P.id
            , P.sex
            , P.first_diagnosis_date
            , P.final_date
            , Y.year
        FROM
            patient_df P
            , years_df Y
        ORDER BY
            P.id
            , Y.year
    ")
    d <- within(d, {
        first_diagnosis_date <- as.Date(first_diagnosis_date)
        final_date <- as.Date(final_date)
        year_starts <- as.Date(paste(year, "-01-01", sep=""))
        year_ends <- as.Date(paste(year, "-12-31", sep=""))
    })
    # Here's the date exclusion:
    if (EXCLUDE_YEARS_NOT_WITHIN_EACH_PATIENT_DATE_RANGE) {
        d <- subset(d,
                    first_diagnosis_date <= year_starts
                        & year_ends <= final_date)
    }
    n <- nrow(d)
    id <- d$id
    year_starts <- d$year_starts
    year_ends <- d$year_ends
    admissions <- numeric(n)
    admission_days <- numeric(n)
    for (i in 1:n) {
        a <- cris$cpft.admissions_between_inclusive(
            admissions_df
            , id[i]
            , year_starts[i]
            , year_ends[i]
        )
        admissions[i] <- a["admissions"]
        admission_days[i] <- a["admission_days"]
    }
    d$admissions <- admissions
    d$admission_days <- admission_days
    return(d)
}

cris$cpft.get_gate_medication_current <- function(
    dbhandle
    , idlist
    , druglist
    , startdate
    , enddate
    , excludepast=FALSE
    , excludenulldose=FALSE)
{
    start_date_as_text <- as.character(startdate)
    end_date_as_text <- as.character(enddate)

    excludepastclause <- ""
    if (excludepast) {
        excludepastclause <- "
            AND tense IS NULL
        "
        # or we might include 'in 1972 they were on blah drug'; tense is 'past' or NULL
    }
    # ... having said that, some past-tense references come through with tense = NULL

    excludenulldoseclause <- ""
    if (excludenulldose) {
        excludenulldoseclause <- "
            AND dose IS NOT NULL
        "
    }

    d <- sqlQuery(dbhandle, paste("
        SET DATEFORMAT dmy; /* for CAST/CONVERT from the varchar date field */
        SELECT
            brcid AS id
            , CAST(date AS DATE) AS date
            , DATEPART(YEAR, CAST(date AS DATE)) AS year
            , drug
            , dose
            , dose_value
            , dose_unit
            , frequency
            , time_unit
            , ", cris$map_drugs_to_uniform_names_sql("drug", druglist), " AS drugname
            , cn_doc_id
            , src_table
            , src_col
            , annotation_start
            , annotation_end
            , tense
        FROM
            GateDB_Camb.dbo.gate_medication_current
        WHERE
            ", cris$sql_id_in_list(idlist, "BrcId"), "
            AND CAST(date AS DATE) IS NOT NULL
            AND CAST(date AS DATE) <> '1900-01-01' /* quasi-NULL value */
            AND CAST(date AS DATE) >= '", start_date_as_text, "'
            AND CAST(date AS DATE) <= '", end_date_as_text, "'
            ", excludepastclause, "
            ", excludenulldoseclause, "
    ", sep=""))

    within(d, {
        date <- as.Date(date)
    })
}

cris$cpft.get_gate_medication_current_by_exclusion_id <- function(
    dbhandle
    , druglist
    , drug_startdate
    , drug_enddate
    , patient_startdate
    , exclusion_idlist)
{
    drug_start_date_as_text <- as.character(drug_startdate)
    drug_end_date_as_text <- as.character(drug_enddate)
    patient_start_date_as_text <- as.character(patient_startdate)
    d <- sqlQuery(dbhandle, paste("
        SET DATEFORMAT dmy; /* for CAST/CONVERT from the varchar date field */
        SELECT
            brcid AS id,
            CAST(date AS DATE) AS date,
            DATEPART(YEAR, CAST(date AS DATE)) AS year,
            drug,
            dose,
            dose_value,
            dose_unit,
            frequency,
            time_unit,
            ", cris$map_drugs_to_uniform_names_sql("drug", druglist), " AS drugname
        FROM
            GateDB_Camb.dbo.gate_medication_current
        WHERE
            brcid IN (
                SELECT brcid
                FROM cpft_endsql.dbo.mpi M
                WHERE
                /* the ID is NOT in a specified list */
                ", cris$sql_id_not_in_list(exclusion_idlist, "M.brcid"), "
                /* We don't want people who died before our period of interest: */
                AND (
                    M.truncated_dttm = 0 /* not dead */
                    OR CAST(M.truncated_dttm AS DATE) >= '", patient_start_date_as_text, "' /* alive at the start */
                )
            )
            AND CAST(date AS DATE) IS NOT NULL
            AND CAST(date AS DATE) <> '1900-01-01' /* quasi-NULL value */
            AND CAST(date AS DATE) >= '", drug_start_date_as_text, "'
            AND CAST(date AS DATE) <= '", drug_end_date_as_text, "'
            AND tense IS NULL /* or we might include 'in 1972 they were on blah drug'; tense is 'past' or NULL */
    ", sep=""))
    d <- subset(d, drugname %in% druglist)
    within(d, {
        date <- as.Date(date)
    })
}

cris$patients_which_drugs_taken <- function(druglist, druginfo_df, patient_df)
{
    # Start: id, date, drug, ...
    # End: id, drug1, drug2, drug3, ... (each cell: boolean was-it-taken)
    result <- data.frame(id=patient_df$id)
    for (drugname in druglist) {
        cat("cris$patients_which_drugs_taken processing:", drugname, "\n")
        tmp <- sqldf(paste("
            SELECT
                id
                , CASE
                    WHEN (
                        SELECT COUNT(*) FROM druginfo_df D
                        WHERE D.id = P.id
                        AND D.drugname = '", drugname, "'
                    ) > 0 THEN 1
                    ELSE 0
                    END AS ", drugname, "
            FROM
                patient_df P
        ", sep=""))
        result <- merge(result, tmp)
    }
    return(result)
}

cris$cpft.per_patient_drug_stats <- function(drugname, druginfo_df, patient_df)
{
    x <- sqldf(paste("
        SELECT
            id
            , sex
            , dob
            , first_diagnosis_date
            , dod
            , died
            , CASE
                WHEN (
                    SELECT COUNT(*) FROM druginfo_df D
                    WHERE D.id = P.id
                    AND D.drugname = '", drugname, "'
                ) > 0 THEN 1
                ELSE 0
                END AS used
            , (
                SELECT date FROM druginfo_df D
                WHERE D.id = P.id
                AND D.drugname = '", drugname, "'
                ORDER BY date ASC
                LIMIT 1
            ) AS first_use
            , (
                SELECT date FROM druginfo_df D
                WHERE D.id = P.id
                AND D.drugname = '", drugname, "'
                ORDER BY date DESC
                LIMIT 1
            ) AS last_use
        FROM
            patient_df P
    ", sep=""))
    within(x, {
        first_use <- cris$date_from_unix_date_in_days(first_use)
        last_use <- cris$date_from_unix_date_in_days(last_use)

        used <- as.logical(used)
        age_at_first_use <- cris$years_between(dob, first_use)
        age_at_last_use <- cris$years_between(dob, last_use)
    })
}

cris$cpft.used_drug_between <- function(druginfo_df, patient_id,
                                        drugname_string, startdate, enddate)
{
    any(
        druginfo_df$id == patient_id
            & druginfo_df$drugname == drugname_string
            & startdate <= druginfo_df$date
            & druginfo_df$date <= enddate
        , na.rm=TRUE
    )
}

cris$cpft.attempt_daily_dose <- function(druginfo_df)
{
    within(druginfo_df, {
        time_unit_days <- ifelse(
            is.na(time_unit) | time_unit == "as required",
            NA,
            ifelse(
                time_unit == "day",
                1,
                ifelse(
                    time_unit == "hour",
                    1/24,
                    ifelse(
                        time_unit == "week",
                        7,
                        ifelse(
                            time_unit == "month",
                            30,
                            NA
                        )
                    )
                )
            )
        )

        daily_dose <- dose_value * frequency / time_unit_days

        conversion_to_mg <- ifelse(
            dose_unit == "mg"
                | dose_unit == "mgs"
                | dose_unit == "mgm"
                | dose_unit == "Mg"
                | dose_unit == "MG"
                | dose_unit == "mgr",
            1,
            ifelse(
                dose_unit == "mcg"
                    | dose_unit == "mcgs"
                    | dose_unit == "micrograms"
                    | dose_unit == "microgrammes"
                    | dose_unit == "microgram"
                    | dose_unit == "microg",
                1/1000,
                ifelse(
                    dose_unit == "g"
                        | dose_unit == "gm"
                        | dose_unit == "gram"
                        | dose_unit == "grams"
                        | dose_unit == "Gm"
                        | dose_unit == "G"
                        | dose_unit == "gr",
                    1000,
                    NA
                )
            )
        )

        daily_dose_mg <- daily_dose * conversion_to_mg
    })
}

cris$cpft.summarize_daily_doses <- function(druginfo_df)
{
    d <- subset(druginfo_df, !is.na(daily_dose_mg) & !is.na(drugname))
    # Down to per-patient level
    d <- ddply(d, .(id, drugname), summarize
        , patient_median_mg_day=median(daily_dose_mg)
        , patient_mean_mg_day=mean(daily_dose_mg)
        , .parallel=TRUE
    )
    # Down to group level
    d <- ddply(d, .(drugname), summarize
        , median_of_median_doses_mg_day=median(patient_median_mg_day)
        , mean_of_mean_doses_mg_day=mean(patient_mean_mg_day)
        , mean_of_median_doses_mg_day=mean(patient_median_mg_day)
        , median_of_mean_doses_mg_day=median(patient_mean_mg_day)
        , .parallel=TRUE
    )
    return(d)
}

#==============================================================================
# Very general graphical methods
#==============================================================================

cris$fixed_effects_from_lmer <- function(
    lmer_result
    , exclude_factors=c()
    , sort_by_effect_size=TRUE
    , strip_true=TRUE
    , alpha=0.05
    , method=c("lmerTest", "z", "pvals.fnc")
)
{
    method <- match.arg(method)
    # lmerTest: something fit with lmerTest::lmer (which overrides lme4::lmer
    #   if the lmerTest package is loaded second)
    # z: something fit with lmer
    # pvals.fnc: something fit with pvals.fnc(lmer(...), ...)

    # Remember: use is(x) to find out what it is class-wise, or str(x) to get
    # a very useful summary.
    # http://stackoverflow.com/questions/1177926/r-object-identification

    # ?lme4::pvalues
    # http://stats.stackexchange.com/questions/73238/alternatives-to-pvals-fnc-to-compute-confidence-intervals-for-fixed-effects

    if (method == "pvals.fnc") {

        # Assumes lmer_result is the output from pvals.fnc
        mcmcdata <- pvals_fnc_result$fixed
        if (strip_true) {
            rownames(mcmcdata) <- gsub("TRUE", "", rownames(mcmcdata))
        }
        d <- data.frame(
            effect=as.factor(rownames(mcmcdata))
            # , estimate=as.numeric(mcmcdata$Estimate) # no, that's the non-MCMC estimate
            , estimate=as.numeric(mcmcdata$MCMCmean)
            , lower=as.numeric(mcmcdata$HPD95lower)
            , upper=as.numeric(mcmcdata$HPD95upper)
        )

    } else if (method == "z" || method == "lmerTest") {

        if (method == "z") {
            # Assumes lmer_result is the output from lmer (an lmer model).
            require(lme4, quietly=TRUE)
            if (!is(lmer_result, "mer")) {
                stop("the input model is not a mer object")
            }
            coefs <- summary(lmer_result)@coefs
        } else {
            # Assumes lmer_result is the output from lmer (an lmer model).
            require(lmerTest, quietly=TRUE)
            if (!is(lmer_result, "merMod")) {
                stop("the input model is not a merMod object")
            }
            coefs <- summary(lmer_result)$coefficients
        }

        # Common code:
        if (strip_true) {
            rownames(coefs) <- gsub("TRUE", "", rownames(coefs))
        }
        if (colnames(coefs)[3] == "z value") {
            d <- data.frame(
                effect=as.factor(rownames(coefs))
                , estimate=coefs[, "Estimate"]
                , se=coefs[, "Std. Error"]
                , z=coefs[, "z value"]
                    # ... which is mean/se (confirmed empirically), since df=1
                , p=coefs[, "Pr(>|z|)"]
            )
        } else if (colnames(coefs)[4] == "t value") {
            d <- data.frame(
                effect=as.factor(rownames(coefs))
                , estimate=coefs[, "Estimate"]
                , se=coefs[, "Std. Error"]
                , df=coefs[, "df"]
                , t=coefs[, "t value"]
                , p=coefs[, "Pr(>|t|)"]
            )
        } else {
            print(coefs)
            stop("need an lmer object producing Z or t values")
        }
        d <- within(d, {
            # Calculate confidence intervals (two-tailed)
            lower <- qnorm(alpha/2, estimate, se)
            upper <- qnorm(1 - alpha/2, estimate, se)
        })

    } else {
        stop("should never get here")
    }

    # At this point, d should be a data frame with:
    #   effect
    #   estimate
    #   lower
    #   upper

    # Flag "significant" effects. We'll call it ci_excludes_zero, but it
    # might be an HPD rather than a CI.
    d$ci_excludes_zero <- sign(d$lower) == sign(d$upper)

    return(d)
}

cris$visualize_fixed_effects_from_preprocessed_lmer <- function(
    d
    , sort_by_effect_size=TRUE
    , title=""
    , xlab="Estimate"
    , limits=c(-Inf, +Inf)
    , visual_style=c("bw", "colour")
)
{
    visual_style <- match.arg(visual_style)
    if (sort_by_effect_size) {
        d$effect <- reorder(d$effect, d$estimate, FUN=mean)
        # without FUN=mean it sometimes does nothing. Try:
        #       a = as.factor(c("a","b","c"))
        #       reorder(a, c(3,2,1))
        #       reorder(a, c(3,2,1), FUN=mean)
        # ... changes level order, but not order of factor values
    }
    # Reverse order (so Y axis reads top-down, not bottom-up)
    d$effect <- factor(d$effect, levels=rev(levels(d$effect)))
    # ... reverses the level order, not the factor value order
    # ... http://stackoverflow.com/questions/2375587/reorder-levels-of-a-data-frame-without-changing-order-of-values

    if (visual_style == "colour") {
        p <- (
            ggplot(data=d,
                   aes(y=effect, x=estimate, colour=factor(ci_excludes_zero)))
            + scale_colour_manual(values=c("black", "red"), guide=FALSE)
        )
    } else {
        p <- (
            ggplot(data=d, aes(y=effect, x=estimate))
            + scale_fill_manual(values=c("white", "black"), guide=FALSE)
        )
    }
    p <- (
        p
        + geom_vline(xintercept=0, colour="grey50")
        + geom_errorbarh(aes(xmin=lower, xmax=upper))
        + geom_point(size=5, shape=21,
                     mapping=aes(fill=factor(ci_excludes_zero)))
    )
    if (limits[1] != -Inf || limits[2] != +Inf) {
        p <- p + coord_cartesian(xlim=limits)
    }
    p <- (
        p
        + xlab(xlab)
        + ylab("")
        + theme_bw()
        + ggtitle(title)
    )
    return(p)
}

cris$visualize_fixed_effects_from_lmer <- function(
    lmer_result
    , exclude_factors=c()
    , sort_by_effect_size=TRUE
    , strip_true=TRUE
    , alpha=0.05
    , method=c("lmerTest", "z", "pvals.fnc")
    , title=""
    , xlab="Estimate"
    , limits=c(-Inf, +Inf)
    , visual_style=c("bw", "colour")
)
{
    d <- cris$fixed_effects_from_lmer(
        lmer_result
        , exclude_factors=exclude_factors
        , strip_true=strip_true
        , alpha=alpha
        , method=method
    )
    return(cris$visualize_fixed_effects_from_preprocessed_lmer(
        d
        , sort_by_effect_size=sort_by_effect_size
        , title=title
        , xlab=xlab
        , limits=limits
        , visual_style=visual_style
    ))
}
#==============================================================================
# Specific analytical methods
#==============================================================================

cris$cpft.print_simple_patient_summary <- function(patient_df)
{
    n <- nrow(patient_df)
    n_male <- nrow(subset(patient_df, sex=="Male"))
    n_female <- nrow(subset(patient_df, sex=="Female"))
    n_died <- nrow(subset(patient_df, died))
    n_died_male <- nrow(subset(patient_df, died & sex=="Male"))
    n_died_female <- nrow(subset(patient_df, died & sex=="Female"))
    mean_age_at_death <- mean(patient_df$age_at_death[
        !is.na(patient_df$age_at_death)])
    mean_age_at_death_male <- mean(patient_df$age_at_death[
        !is.na(patient_df$age_at_death) & patient_df$sex == "Male"])
    mean_age_at_death_female <- mean(patient_df$age_at_death[
        !is.na(patient_df$age_at_death) & patient_df$sex == "Female"])
    chisq_sex <- chisq.test(c(n_male, n_female))
    patient_df_sex_mf <- subset(patient_df, sex=="Male" | sex=="Female")
    # ... could also be not known/not specified/X...
    ttest_age_at_death <- t.test(age_at_death ~ sex, data=patient_df_sex_mf)

    cat(BIGSEP)
    cat("Number of patients in this group:", n, "\n")
    cat("... male:", n_male, "\n")
    cat("... female:", n_female, "\n")
    cat("... chi-square test:\n")
    print(chisq_sex)
    cat("... exact p-value for that:", chisq_sex$p.value)
    cat(BIGSEP)
    cat("n_died:", n_died, "\n")
    cat("n_died_male:", n_died_male, "\n")
    cat("n_died_female:", n_died_female, "\n")
    cat("Mean age at death:", mean_age_at_death, "\n")
    cat("Mean age at death (male):", mean_age_at_death_male, "\n")
    cat("Mean age at death (female):", mean_age_at_death_female, "\n")
}

cris$cpft.admissions_by_year_plot_by_patient <- function(
    admissions_by_patient_year_df, firstyear, lastyear)
{
    # Pass in an admissions df that DOES exclude years not within each patient's date range
    SUMMARY_ADMDAYS_BY_YEAR <- ddply(
        admissions_by_patient_year_df
        , ~ sex * year
        , summarize
        , mean_admission_days_per_pt=mean(admission_days)
        , total_admission_days=sum(admission_days)
        , .parallel=TRUE
    )
    return(
        ggplot(
            data=SUMMARY_ADMDAYS_BY_YEAR
            , aes(x=year, y=mean_admission_days_per_pt, linetype=sex)
        )
        + geom_line()
        + scale_y_continuous(
            "Mean admission days per patient per year",
            limits=c(0,max(SUMMARY_ADMDAYS_BY_YEAR$mean_admission_days_per_pt)))
        + scale_x_continuous("Year", limits=c(firstyear, lastyear))
        + theme_bw()
        + theme(legend.position=c(0.75, 0.75))
    )
}

cris$cpft.admissions_by_year_plot_as_institution_proportion <- function(
    admissions_by_patient_year_df,
    available_beds_by_year_df,
    firstyear,
    lastyear
)
{
    # Pass in an admissions df that does NOT exclude years not within each patient's date range
    SUMMARY_ADMDAYS_TOTAL_BY_YEAR <- ddply(
        admissions_by_patient_year_df
        , ~ year
        , summarize
        , total_admission_days=sum(admission_days)
        , .parallel=TRUE
    )
    SUMMARY_ADMDAYS_TOTAL_BY_YEAR <- merge(SUMMARY_ADMDAYS_TOTAL_BY_YEAR,
                                           available_beds_by_year_df)
    SUMMARY_ADMDAYS_TOTAL_BY_YEAR <- within(SUMMARY_ADMDAYS_TOTAL_BY_YEAR, {
        available_bed_days <- beds * cris$DAYS_PER_YEAR
        proportion_bed_days_used <- total_admission_days / available_bed_days
    })
    return(
        ggplot(
            data=SUMMARY_ADMDAYS_TOTAL_BY_YEAR
            , aes(x=year, y=proportion_bed_days_used)
        )
        + geom_line()
        + scale_y_continuous("Proportion of available bed-days used",
                             limits=c(0,1))
        + scale_x_continuous("Year", limits=c(firstyear, lastyear))
        + theme_bw()
    )
}

cris$cpft.simple_drug_stats <- function(drugname, druginfo, patient_df)
{
    cat("cris$cpft.simple_drug_stats:", drugname, "\n")
    P <- cris$cpft.per_patient_drug_stats(drugname, druginfo, patient_df)
    n_pts <- nrow(P)
    n_male <- nrow(subset(P, sex == "Male"))
    n_female <- nrow(subset(P, sex == "Female"))

    n_used <- nrow(subset(P, used))
    n_did_not_use <- nrow(subset(P, !used))
    n_used_male <- nrow(subset(P, used & sex == "Male"))
    n_used_female <- nrow(subset(P, used & sex == "Female"))
    n_used_died <- nrow(subset(P, used & died))
    n_used_did_not_die <- nrow(subset(P, used & !died))
    n_did_not_use_died <- nrow(subset(P, !used & died))
    n_did_not_use_did_not_die <- nrow(subset(P, !used & !died))

    pval_use_by_sex <- ifelse(n_used > 0,
                              chisq.test(P$used, P$sex)$p.value,
                              NA)
    pval_use_by_death <- ifelse(n_used > 0,
                                chisq.test(P$used, P$died)$p.value,
                                NA)

    return(data.frame(
        drugname
        , n_pts
        , n_used
        , proportion_used=(n_used / n_pts)
        , percentage_used=round(100 * n_used / n_pts, 1)
        , n_did_not_use
        , n_used_male
        , n_used_female
        , n_used_died
        , n_used_did_not_die
        , n_did_not_use_died
        , n_did_not_use_did_not_die

        , pval_use_by_sex
        , sex_sig=(pval_use_by_sex <= 0.05)
        , pval_use_by_death
        , death_sig=(pval_use_by_death <= 0.05)

        , p_used_given_male=(n_used_male / n_male)
        , p_used_given_female=(n_used_female / n_female)

        , p_died_given_drug=(n_used_died / n_used)
        , p_died_given_no_drug=(n_did_not_use_died / n_did_not_use)

        , mean_age_at_first_use=mean(P$age_at_first_use, na.rm=TRUE)
    ))
}

cris$cpft.drug_use_by_diagnosis <- function(drugname,
                                            druginfo_df_diagnosis,
                                            patient_df_diagnosis,
                                            druginfo_df_no_diagnosis,
                                            patient_df_no_diagnosis)
{
    DP <- cris$cpft.per_patient_drug_stats(drugname,
                                           druginfo_df_diagnosis,
                                           patient_df_diagnosis)
    DP$diagnosis <- TRUE
    NP <- cris$cpft.per_patient_drug_stats(drugname,
                                           druginfo_df_no_diagnosis,
                                           patient_df_no_diagnosis)
    NP$diagnosis <- FALSE
    n_diagnosis <- nrow(DP)
    n_no_diagnosis <- nrow(NP)
    n_used_diagnosis <- nrow(subset(DP, used))
    n_used_no_diagnosis <- nrow(subset(NP, used))
    used <- c(DP$used, NP$used)
    diagnosis <- c(DP$diagnosis, NP$diagnosis)
    pval_use_by_diagnosis <- ifelse(
        n_used_diagnosis > 0
            & n_used_no_diagnosis > 0  # fixed 2015-07-09: was &&
            & n_diagnosis > 0  # fixed 2015-07-09: was &&
            & n_no_diagnosis
        , chisq.test(used, diagnosis)$p.value
        , NA
    )
    return(data.frame(
        drugname
        , n_diagnosis=n_diagnosis
        , n_no_diagnosis=n_no_diagnosis
        , n_used_diagnosis=n_used_diagnosis
        , n_used_no_diagnosis=n_used_no_diagnosis
        , p_used_given_diagnosis=(n_used_diagnosis / n_diagnosis)
        , p_used_given_no_diagnosis=(n_used_no_diagnosis / n_no_diagnosis)
        , pval_use_by_diagnosis
        , diagnosis_sig=(pval_use_by_diagnosis <= 0.05)
    ))
}

cris$cpft.admissions_by_drug_onset_mirror <- function(
    drugname, druginfo_df, patient_df, admissions_df
    , mirroryears_before
    , mirroryears_after
    , CRIS_EXPORT_DATE_AS_DATE
    , central_gap_days_before=1
    , central_gap_days_after=1
    , min_component_length_days=NA)
{
    # Returns data frame for a single drug
    # Use mirroryears=NA for "as far back/forward as possible" -- in which case specify min_component_length_days (or a single day that's an admission before drug will give a measure of about 365 days/year beforehand)
    cat("cris$cpft.admissions_by_drug_onset_mirror:", drugname, "\n")
    mirrordays_before <- mirroryears_before * cris$DAYS_PER_YEAR
    mirrordays_after <- mirroryears_after * cris$DAYS_PER_YEAR
    P <- cris$cpft.per_patient_drug_stats(drugname, druginfo_df, patient_df)
    # Restrict to those who used the drug
    P <- subset(P, used)
    P <- within(P, {
        # We don't care about time before diagnosis:
        start_date <- cris$date_from_unix_date_in_days(
            ifelse(FIRST_DATE_AS_DATE > first_diagnosis_date,
                   FIRST_DATE_AS_DATE,
                   first_diagnosis_date)
        )
        # We don't care about time after death:
        end_date <- cris$date_from_unix_date_in_days(
            ifelse(!is.na(dod) & dod < CRIS_EXPORT_DATE_AS_DATE,
                   dod,
                   CRIS_EXPORT_DATE_AS_DATE)
        )
        mirror_1_end   <- first_use - central_gap_days_before
        mirror_2_start <- first_use + central_gap_days_after
        if (is.na(mirroryears_before)) { # "As long as possible" mirror
            mirror_1_start <- start_date
        }
        else { # Conventional mirror
            mirror_1_start <- first_use - central_gap_days_before - mirrordays_before
        }
        if (is.na(mirroryears_after)) { # "As long as possible" mirror
            mirror_2_end <- end_date
        }
        else { # Conventional mirror
            mirror_2_end <- first_use + central_gap_days_after + mirrordays_after
        }
        # "Stringent" drug requirement is the second half of the second bit of the mirror
        stringent_drug_req_start <- mirror_2_start + (mirror_2_end - mirror_2_start)/2
        stringent_drug_req_end   <- mirror_2_end
    })
    # Exclude those where the mirror span falls outside the start/end dates
    # (i.e. not those who took drug within N years of start date, and not those who only began drug within N years of end date).
    P <- subset(P, start_date <= mirror_1_start & mirror_2_end <= end_date)
    # Exclude anything silly
    P <- subset(P, mirror_1_start <= mirror_1_end & mirror_2_start <= mirror_2_end)
    if (!is.na(min_component_length_days)) {
        P <- subset(P, cris$days_between(mirror_1_start, mirror_1_end) >= min_component_length_days)
        P <- subset(P, cris$days_between(mirror_2_start, mirror_2_end) >= min_component_length_days)
    }

    n <- nrow(P)
    adm_before <- numeric(n)
    adm_after <- numeric(n)
    adm_days_before <- numeric(n)
    adm_days_after <- numeric(n)
    used_in_stringent_period <- logical(n)
    allpts_adm_before <- numeric(n)
    allpts_adm_after <- numeric(n)
    allpts_adm_days_before <- numeric(n)
    allpts_adm_days_after <- numeric(n)
    id <- P$id
    mirror_1_start <- P$mirror_1_start
    mirror_1_end <- P$mirror_1_end
    mirror_2_start <- P$mirror_2_start
    mirror_2_end <- P$mirror_2_end
    stringent_drug_req_start <- P$stringent_drug_req_start
    stringent_drug_req_end <- P$stringent_drug_req_end
    for (i in 1:n) {
        a1 <- cris$cpft.admissions_between_inclusive(admissions_df,
                                                     id[i],
                                                     mirror_1_start[i],
                                                     mirror_1_end[i])
        adm_before[i] <- a1["admissions"]
        adm_days_before[i] <- a1["admission_days"]
        a2 <- cris$cpft.admissions_between_inclusive(admissions_df,
                                                     id[i],
                                                     mirror_2_start[i],
                                                     mirror_2_end[i])
        adm_after[i] <- a2["admissions"]
        adm_days_after[i] <- a2["admission_days"]
        used_in_stringent_period[i] <- cris$cpft.used_drug_between(
            druginfo_df
            , id[i]
            , drugname
            , stringent_drug_req_start[i]
            , stringent_drug_req_end[i]
        )

        allpts1 <- cris$cpft.admissions_between_inclusive(admissions_df,
                                                          NA,
                                                          mirror_1_start[i],
                                                          mirror_1_end[i]) # all patients
        allpts_adm_before[i] <- allpts1["admissions"]
        allpts_adm_days_before[i] <- allpts1["admission_days"]
        allpts2 <- cris$cpft.admissions_between_inclusive(admissions_df,
                                                          NA,
                                                          mirror_2_start[i],
                                                          mirror_2_end[i]) # all patients
        allpts_adm_after[i] <- allpts2["admissions"]
        allpts_adm_days_after[i] <- allpts2["admission_days"]
    }

    P$adm_before <- adm_before
    P$adm_after <- adm_after
    P$adm_days_before <- adm_days_before
    P$adm_days_after <- adm_days_after
    P$used_in_stringent_period <- used_in_stringent_period
    P$allpts_adm_before <- allpts_adm_before
    P$allpts_adm_after <- allpts_adm_after
    P$allpts_adm_days_before <- allpts_adm_days_before
    P$allpts_adm_days_after <- allpts_adm_days_after

    within(P, {
        drugname <- drugname
        planned_mirroryears_before <- mirroryears_before
        planned_mirroryears_after  <- mirroryears_after
        mirror_years_before <- cris$years_between(mirror_1_start, mirror_1_end)
        mirror_years_after  <- cris$years_between(mirror_2_start, mirror_2_end)

        adm_per_year_before <- adm_before / mirror_years_before
        adm_per_year_after <- adm_after / mirror_years_after
        adm_days_per_year_before <- adm_days_before / mirror_years_before
        adm_days_per_year_after <- adm_days_after / mirror_years_after

        change_adm_days_per_year <- adm_days_per_year_after - adm_days_per_year_before

        allpts_adm_per_year_before <- allpts_adm_before / mirror_years_before
        allpts_adm_per_year_after <- allpts_adm_after / mirror_years_after
        allpts_adm_days_per_year_before <- allpts_adm_days_before / mirror_years_before
        allpts_adm_days_per_year_after <- allpts_adm_days_after / mirror_years_after

        corrected_for_allpts_change_adm_days_per_year <- (adm_days_per_year_after - allpts_adm_days_per_year_after) - (adm_days_per_year_before - allpts_adm_days_per_year_before)
    })
}

cris$summarize_drug_mirror <- function(MIRROR, ci=0.95, only_if_adm_before=FALSE, stringent=FALSE, patient_idlist=NA)
{
    cat("cris$summarize_drug_mirror:", MIRROR$drugname[1], "\n")
    if (only_if_adm_before) {
        MIRROR <- subset(MIRROR, adm_days_before > 0)
    }
    if (!is.na(patient_idlist[1])) {
        MIRROR <- subset(MIRROR, id %in% patient_idlist)
    }
    if (stringent) {
        MIRROR <- subset(MIRROR, used_in_stringent_period)
    }
    data.frame(
        drugname=MIRROR$drugname[1] # same for all rows
        , mirroryears_before=MIRROR$planned_mirroryears_before[1] # same for all rows
        , mirroryears_after=MIRROR$planned_mirroryears_after[1] # same for all rows
        , n_pts=nrow(MIRROR)
        , mean_age=mean(MIRROR$age_at_first_use)
        , n_male=nrow(subset(MIRROR, sex == "Male"))

        , mean_adm_per_year_before=mean(MIRROR$adm_per_year_before)
        , mean_adm_per_year_after=mean(MIRROR$adm_per_year_after)
        , pvalue_adms=t.test(MIRROR$adm_per_year_before, MIRROR$adm_per_year_after, paired=TRUE)$p.value

        , mean_adm_days_per_year_before=mean(MIRROR$adm_days_per_year_before)
        , mean_adm_days_per_year_after=mean(MIRROR$adm_days_per_year_after)
        , mean_adm_days_per_year_change=mean(MIRROR$change_adm_days_per_year)
        , sem_adm_days_change=sem(MIRROR$change_adm_days_per_year)
        , hci95_adm_days_change=half_confidence_interval_t(MIRROR$change_adm_days_per_year, ci=ci)
        , pvalue_adm_days=t.test(MIRROR$adm_days_per_year_before, MIRROR$adm_days_per_year_after, paired=TRUE)$p.value

        , mean_corrected_adm_days_per_year_change=mean(MIRROR$corrected_for_allpts_change_adm_days_per_year)
        , sem_corrected_adm_days_change=sem(MIRROR$corrected_for_allpts_change_adm_days_per_year)
        , hci95_corrected_adm_days_change=half_confidence_interval_t(MIRROR$corrected_for_allpts_change_adm_days_per_year, ci=ci)
        , pvalue_corrected_adm_days=t.test(MIRROR$corrected_for_allpts_change_adm_days_per_year)$p.value
    )
}

cris$drug_mirror_plot <- function(MIRROR_SUMMARY, DRUGLIST=NA, corrected=FALSE, sort_by_effect_size=TRUE, title="", visual_style=1)
{
    # second y axis tricky: http://rpubs.com/kohske/dual_axis_in_ggplot2
    if (!is.na(DRUGLIST[1])) { # pass NA for all drugs, or specify a list
        MIRROR_SUMMARY <- subset(MIRROR_SUMMARY, drugname %in% DRUGLIST)
    }
    # Eliminate the effects of "not analysable" drugs
    MIRROR_SUMMARY <- subset(MIRROR_SUMMARY,
                             !is.na(pvalue_adm_days)
                                & !is.na(mean_adm_days_per_year_before)
                                & !is.na(mean_adm_days_per_year_after))
    MIRROR_SUMMARY <- within(MIRROR_SUMMARY, {
        # Append n
        drugname <- paste(drugname, " (n=", n_pts, ", ",
                          round(mean_age, digits=1), "y, ",
                          n_male, " M)", sep="")
        # Which sort of analysis?
        if (corrected) {
            depvar <- mean_corrected_adm_days_per_year_change
            hci <- hci95_corrected_adm_days_change
            if (sort_by_effect_size) {
                drugname <- reorder(drugname,
                                    mean_corrected_adm_days_per_year_change,
                                    FUN=mean) # Sort drug factor order
            }
        }
        else {
            depvar <- mean_adm_days_per_year_change
            hci <- hci95_adm_days_change
            if (sort_by_effect_size) {
                drugname <- reorder(drugname,
                                    mean_adm_days_per_year_change,
                                    FUN=mean) # Sort drug factor order
            }
        }
        # Reverse order (so Y axis reads top-down, not bottom-up)
        drugname <- factor(drugname, levels=rev(levels(drugname)))
        # Flag "significant" effects
        ci_excludes_zero <- sign(depvar + hci) == sign(depvar - hci)
    })

    if (visual_style == 1) {
        return(
            ggplot(
                data=MIRROR_SUMMARY
                , aes(y=drugname, x=depvar, colour=factor(ci_excludes_zero))
            )
            + geom_vline(xintercept=0, colour="grey50")
            + geom_errorbarh(aes(xmin=depvar - hci, xmax=depvar + hci))
            + geom_point(size=5)
            + xlab("Change in admission days per year")
            + ylab("")
            + scale_colour_manual(values=c("black", "red"), guide=FALSE)
            + theme_bw()
            # + theme(axis.text.x=element_text(angle=90, hjust=1))
            + ggtitle(title)
        )
    } else {
        return(
            ggplot(data=MIRROR_SUMMARY, aes(y=drugname, x=depvar))
            + geom_vline(xintercept=0, colour="grey50")
            + geom_errorbarh(aes(xmin=depvar - hci, xmax=depvar + hci))
            + geom_point(size=5, shape=21,
                         mapping=aes(fill=factor(ci_excludes_zero)))
            + xlab("Change in admission days per year")
            + ylab("")
            + scale_fill_manual(values=c("white", "black"), guide=FALSE)
            + theme_bw()
            # + theme(axis.text.x=element_text(angle=90, hjust=1))
            + ggtitle(title)
        )
    }
}

cris$drug_mirror_stringent_comparison <- function(MIRROR,
                                                  only_if_adm_before=TRUE)
{
    # Asks the question: for those that used a drug, is the impact on admission days
    # affected by whether they clearly used the drug in the second half of the post-drug phase
    # (i.e. remained on it)?
    if (only_if_adm_before) {
        MIRROR <- subset(MIRROR, adm_days_before > 0)
    }
    data <- melt(
        data=MIRROR
        , id.vars=c("id", "used_in_stringent_period") # subject ID and between-subject variables
        , measure.vars=c("adm_days_before", "adm_days_after") # "wide" variables to be made "long"
        , variable.name="phase" # equivalent "long" variable
        , value.name="adm_days" # dependent variable
    )
    ezANOVA(
        data=data
        , dv=adm_days
        , wid=id
        , within=.(phase)
        , between=.(used_in_stringent_period)
        , type=3
    )
}

cris$admissions_and_drug_use_by_calendar_periods <- function(
    patient_df
    , druginfo_df
    , admissions_df
    , druglist
    , startdate
    , enddate
    , n_chunks
    , drug_in_preceding_period=TRUE
    , drug_in_preceding_or_same_period=FALSE
    )
{
    # Optimizing: http://stackoverflow.com/questions/2908822/speed-up-the-loop-operation-in-r
    cat("cris$admissions_and_drug_use_by_calendar_periods...\n")
    druginfo_df <- subset(druginfo_df, drugname %in% druglist) # for speed later
    timespan_days <- cris$days_between(startdate, enddate)
    chunk_size <- timespan_days/n_chunks
    period_start <- cris$round_date_to_days(startdate + chunk_size
                                            * seq(0, n_chunks - 1))
    period_end <- cris$round_date_to_days(
        c(period_start[2:length(period_start)] - 1,
          enddate))
    n_periods <- length(period_start)
    if (drug_in_preceding_or_same_period) {
        # drug use in the SAME OR THE IMMEDIATELY PRECEDING PERIOD
        calendar_periods_df <- data.frame(
            period_num=2 : n_periods
            , period_start=period_start[2 : n_periods]
            , period_end=period_end[2 : n_periods]
            , period_length_days=as.numeric(
                period_end[2 : n_periods]
                - period_start[2 : n_periods]
                + 1
            )
            , relevant_drug_period_start=period_start[1 : (n_periods - 1)]
            , relevant_drug_period_end=period_end[2 : n_periods] # NB. Each drug period covers two measurement periods.
        )
    } else {
        if (drug_in_preceding_period) {
            # Default -- Drug use is: drug use in the immediately PRECEDING period
            calendar_periods_df <- data.frame(
                period_num=2 : n_periods
                , period_start=period_start[2 : n_periods]
                , period_end=period_end[2 : n_periods]
                , period_length_days=as.numeric(
                    period_end[2 : n_periods]
                    - period_start[2 : n_periods]
                    + 1
                )
                , relevant_drug_period_start=period_start[1 : (n_periods - 1)]
                , relevant_drug_period_end=period_end[1 : (n_periods - 1)]
            )
        } else {
            # drug use in SAME period (so: gain one extra period of measurement, the first, which we had to ignore when looking at drug use in the preceding period)
            calendar_periods_df <- data.frame(
                period_num=1 : n_periods
                , period_start=period_start[1 : n_periods]
                , period_end=period_end[1 : n_periods]
                , period_length_days=as.numeric(
                    period_end[1 : n_periods]
                    - period_start[1 : n_periods]
                    + 1
                )
                , relevant_drug_period_start=period_start[1 : n_periods]
                , relevant_drug_period_end=period_end[1 : n_periods]
            )
        }
    }
    data <- sqldf("
        SELECT
            P.id
            , P.sex
            , P.first_diagnosis_date
            , P.dob
            , C.period_num
            , C.period_start
            , C.period_end
            , C.period_length_days
            , C.relevant_drug_period_start
            , C.relevant_drug_period_end
        FROM
            patient_df P
            , calendar_periods_df C
        WHERE
            P.first_diagnosis_date <= C.period_start
            AND P.final_date >= C.period_end
        ORDER BY
            P.id
            , C.period_start
    ")
    data <- within(data, {
        dob <- as.Date(dob)
        period_start <- as.Date(period_start)
        period_end <- as.Date(period_end)
        age <- cris$years_between(dob,
                                  cris$date_halfway_between(period_start,
                                                            period_end))
        relevant_drug_period_start <- as.Date(relevant_drug_period_start)
        relevant_drug_period_end <- as.Date(relevant_drug_period_end)
        first_diagnosis_date <- as.Date(first_diagnosis_date)
        estimated_illness_duration <- cris$years_between(
            first_diagnosis_date,
            cris$date_halfway_between(period_start, period_end))
    })

    cat("cris$admissions_and_drug_use_by_calendar_periods...",
        "collecting admission information\n")
    n <- nrow(data)
    patient_id <- data$id
    period_start <- data$period_start
    period_end <- data$period_end
    admissions <- numeric(n)
    admission_days <- numeric(n)
    for (i in 1:n) {
        if (i %% 100 == 0) cat("... admissions for record", i, "of", n, "\n")
        a <- cris$cpft.admissions_between_inclusive(admissions_df,
                                                    patient_id[i],
                                                    period_start[i],
                                                    period_end[i])
        admissions[i] <- a["admissions"]
        admission_days[i] <- a["admission_days"]
    }
    data$admissions <- admissions
    data$admission_days <- admission_days
    data <- within(data, {
        admission_days_per_year <- admission_days * cris$DAYS_PER_YEAR / period_length_days
    })

    # Parallel from 2015-07-09
    resultlist <- foreach(i=1:length(druglist)) %dopar% {
        drugname <- druglist[i]
        cat("cris$admissions_and_drug_use_by_calendar_periods... processing",
            drugname, "\n")
        # *VERY* much faster to farm this out to SQL.
        drugresult <- sqldf(paste("
            SELECT
                data.id
                , data.period_start
                , (
                    SELECT
                        COUNT(*)
                    FROM
                        druginfo_df
                    WHERE
                        druginfo_df.id = data.id
                        AND druginfo_df.drugname = '", drugname, "'
                        AND druginfo_df.date >= data.relevant_drug_period_start
                        AND druginfo_df.date <= data.relevant_drug_period_end
                ) AS used
            FROM
                data
            ORDER BY
                data.id
                , data.period_start
        ", sep=""))
        return(drugresult$used > 0)
    }
    # Now reassemble...
    for (i in 1:length(druglist)) {
        drugname <- druglist[i]
        data[drugname] <- resultlist[[i]]
    }
    return(data)
}

cris$admissions_and_drug_use_by_day <- function(patient_df, druginfo_df,
                                                admissions_df, druglist,
                                                start_date, end_date)
{
    cat("cris$admissions_and_drug_use_by_day...\n")
    n_patients <- nrow(patient_df)
    alldates <- cris$date_range_inclusive(start_date, end_date)
    n_dates <- length(alldates)
    n_everything <- n_patients * n_dates
    patient_ids <- patient_df$id
    sexes <- patient_df$sex
    first_diagnosis_dates <- patient_df$first_diagnosis_date
    cat("cris$admissions_and_drug_use_by_day.. building big data frame\n")
    big_df <- data.frame(
        patient_id=rep(patient_ids, each=n_dates)
        , sex=rep(sexes, each=n_dates)
        , date=rep(alldates, times=n_patients)
        , admitted=numeric(n_everything) # zeros
    )
    for (drugname in DRUGLIST) {
        big_df[drugname] <- numeric(n_everything) # zeros
    }

    # Process admissions
    admitted <- numeric(n_everything)
    admission_pids <- admissions_df$id
    admission_dates <- admissions_df$admission_date
    discharge_dates <- admissions_df$discharge_date
    for (i in seq_along(admission_dates)) {
        cat("admission number", i, "\n")
        adm <- admission_dates[i]
        dis <- discharge_dates[i]
        if (adm > end_date) next
        if (dis < start_date) next
        patient_offset <- (which(patient_ids == admission_pids[i]) - 1) * n_dates + 1
        start_index <- ifelse(adm < first_date,
                              1,
                              which(alldates == admission_dates[i]))
        end_index   <- ifelse(dis > last_date,
                              n,
                              which(alldates == discharge_dates[i]))
        admitted[ (patient_offset + start_index) : (patient_offset + end_index) ] <- 1
    }
    big_df$admitted <- admitted

    # Process drugs
    # *** PLAN WAS: when a drug appears, mark the next N days as being "on" drug.
    # *** INCOMPLETE - "cannot allocate vector of size 33.1 Mb" when first creating big_df

    return(big_df)
}

cris$age_at_death_by_year_diagnosis <- function(dead_patient_df, ci=0.95)
{
    cat("cris$age_at_death_by_year_diagnosis\n")
    data <- ddply(
        dead_patient_df
        , ~ sex * yod * diagnosis
        , function(chunkdf) { # if we use summarize, variables like ci are hidden
            data.frame(
                mean_age_at_death=mean(chunkdf$age_at_death),
                hci=half_confidence_interval_t(chunkdf$age_at_death, ci=ci)
            )
        }
        , .parallel=TRUE
    )
    cat("cris$age_at_death_by_year_diagnosis -- ddply done\n")
    return(
        ggplot(
            data=data
            , aes(x=yod, y=mean_age_at_death, linetype=diagnosis, colour=sex)
        )
        + geom_ribbon(aes(ymin=mean_age_at_death - hci,
                          ymax=mean_age_at_death + hci, fill=sex),
                      alpha=0.1)
        + geom_line(size=2)
        + scale_x_continuous("Year of death")
        + scale_y_continuous("Age at death")
        + theme_bw()
    )
}

#==============================================================================
# Concurrent drug use (depending on the exact definition of "concurrent").
#==============================================================================
# Conceptually easy when:
# (a) if you've ever taken A, have you ever taken B?
# (b) if you took A at time X, did you take B at time X +/- Y?
# However, harder in the general sense:
# (c) for a range of time periods, did you take A in each, and if so, did you take B in the same time period?
#     ... more complex because e.g. then have to obtain a per-patient average (across time periods),
#     before combining across patients.

cris$TIMESCALE_OPTIONS <- c("ever") # , "same_calendar_year")

cris$p_ever_took_drug_B_if_ever_took_drug_A <- function(druginfo_df, drug_A,
                                                        drug_B)
{
    patients_who_took_A <- unique(subset(druginfo_df, drugname == drug_A)$id )
    n_patients_who_took_A <- length(patients_who_took_A)
    patients_who_took_both <- unique(subset(druginfo_df, drugname == drug_B & id %in% patients_who_took_A)$id )
    n_patients_who_took_both <- length(patients_who_took_both)
    return(n_patients_who_took_both / n_patients_who_took_A)
}

cris$concurrent_drug_use <- function(druginfo_df, druglist,
                                     timescale=cris$TIMESCALE_OPTIONS)
{
    cat("cris$concurrent_drug_use...\n")
    n_drugs <- length(druglist)
    m <- matrix(NA, nrow=n_drugs, ncol=n_drugs)
    rownames(m) <- paste("IF_TOOK", druglist)
    colnames(m) <- paste("P_TOOK", druglist)
    for (r in 1:n_drugs) {
        cat("Processing drug", r, "of", n_drugs, "...\n")
        for (c in 1:n_drugs) {
            drug_A <- druglist[r]
            drug_B <- druglist[c]
            if (timescale == "ever") {
                m[r, c] <- cris$p_ever_took_drug_B_if_ever_took_drug_A(
                    druginfo_df, drug_A, drug_B)
            }
        }
    }
    return(m)
}

#==============================================================================
# Drug use prior to admission
#==============================================================================

cris$drug_use_prior_to_admission <- function(admission_df, druginfo_df,
                                             druglist, time_before_days=90)
{
    # admission_df should be corrected for back-to-back admissions ***

    d <- admission_df
    d$drug_window_start_date <- d$admission_date - time_before_days
    d$drug_window_end_date <- d$admission_date - 1
    # Parallel from 2015-07-09
    resultlist <- foreach(i=1:length(druglist)) %dopar% {
        drugname <- druglist[i]
        cat("processing", drugname, "...\n")
        result <- sqldf(paste("
            SELECT
                id
                , admission_date
                , EXISTS(
                    SELECT * FROM druginfo_df AS dr
                    WHERE dr.id = d.id
                    AND dr.drugname = '", drugname, "'
                    AND dr.date >= d.drug_window_start_date
                    AND dr.date <= d.drug_window_end_date
                ) AS used_drug
            FROM
                d
        ", sep=""))

        return(result$used_drug)
    }
    # Now reassemble...
    for (i in 1:length(druglist)) {
        drugname <- druglist[i]
        d[drugname] <- resultlist[[i]]
    }
    return(d)
}

#==============================================================================
# Drug mortality
#==============================================================================

cris$death_and_ever_used_drug <- function(patient_df, druginfo_df, druglist,
                                          startdate, enddate)
{
    cat("cris$death_and_ever_used_drug...\n")
    druginfo_df <- subset(druginfo_df, drugname %in% druglist) # for speed later
    data <- subset(patient_df, is.na(dod) || dod >= startdate) # exclude those who died before we start
    data <- data[ with(data, order(id)), ] # order by id
    data <- within(data, {
        startdate <- startdate
        enddate <- enddate
    })

    for (drugname in druglist) {
        cat("cris$death_and_ever_used_drug... processing", drugname, "\n")
        drugresult <- sqldf(paste("
            SELECT
                data.id
                , (
                    SELECT
                        COUNT(*)
                    FROM
                        druginfo_df
                    WHERE
                        druginfo_df.id = data.id
                        AND druginfo_df.drugname = '", drugname, "'
                        AND druginfo_df.date >= data.startdate
                        AND druginfo_df.date <= data.enddate
                ) AS used
            FROM
                data
            ORDER BY
                data.id
        ", sep=""))
        data[drugname] <- (drugresult$used > 0)
    }
    return(data)
}

cris$drug_use_death_by_calendar_periods <- function(patient_df, druginfo_df,
                                                    druglist, startdate,
                                                    enddate, n_chunks)
{
    # Drug use is: drug use in the immediately PRECEDING period OR the CURRENT period
    cat("cris$drug_use_death_by_calendar_periods...\n")
    druginfo_df <- subset(druginfo_df, drugname %in% druglist) # for speed later
    timespan_days <- cris$days_between(startdate, enddate)
    chunk_size <- timespan_days/n_chunks
    period_start <- cris$round_date_to_days(startdate + chunk_size * seq(0, n_chunks - 1))
    period_end <- cris$round_date_to_days(
        c(period_start[2:length(period_start)] - 1,
          enddate))
    n_periods <- length(period_start)
    calendar_periods_df <- data.frame(
        period_num=2 : n_periods
        , period_start=period_start[2 : n_periods]
        , period_end=period_end[2 : n_periods]
        , period_length_days=as.numeric(
            period_end[2 : n_periods]
            - period_start[2 : n_periods]
            + 1
        )
        , preceding_period_start=period_start[1 : (n_periods - 1)]
        , preceding_period_end=period_end[1 : (n_periods - 1)]
    )
    data <- sqldf("
        SELECT
            P.id
            , P.sex
            , P.dob
            , P.dod
            , P.first_diagnosis_date
            , C.period_num
            , C.period_start
            , C.period_end
            , C.period_length_days
            , C.preceding_period_start
            , C.preceding_period_end
        FROM
            patient_df P
            , calendar_periods_df C
        WHERE
            P.first_diagnosis_date <= C.period_start
            /* this time, we do NOT exclude periods in which the patient died; we exclude periods ENTIRELY AFTER the patient died */
            AND P.final_date >= C.period_start
        ORDER BY
            P.id
            , C.period_start
    ")
    data <- within(data, {
        dob <- as.Date(dob)
        dod <- as.Date(dod)
        period_start <- as.Date(period_start)
        period_end <- as.Date(period_end)
        preceding_period_start <- as.Date(preceding_period_start)
        preceding_period_end <- as.Date(preceding_period_end)
        first_diagnosis_date <- as.Date(first_diagnosis_date)
        estimated_illness_duration <- cris$years_between(
            first_diagnosis_date,
            cris$date_halfway_between(period_start, period_end))

        age <- cris$years_between(dob,
                                  cris$date_halfway_between(period_start,
                                                            period_end))

        died_in_period <- !is.na(dod) & dod >= period_start & dod <= period_end
    })

    # Parallel from 2015-07-09
    resultlist <- foreach(i=1:length(druglist)) %dopar% {
        drugname <- druglist[i]
        cat("cris$drug_use_death_by_calendar_periods... processing",
            drugname, "\n")
        # *VERY* much faster to farm this out to SQL.
        drugresult <- sqldf(paste("
            SELECT
                data.id
                , data.period_start
                , (
                    SELECT
                        COUNT(*)
                    FROM
                        druginfo_df
                    WHERE
                        druginfo_df.id = data.id
                        AND druginfo_df.drugname = '", drugname, "'
                        AND druginfo_df.date >= data.preceding_period_start /* from the start of the preceding period... */
                        AND druginfo_df.date <= data.period_end             /* ... to the end of the period in question. */
                ) AS used
            FROM
                data
            ORDER BY
                data.id
                , data.period_start
        ", sep=""))
        return(drugresult$used > 0)
    }
    # Now reassemble...
    for (i in 1:length(druglist)) {
        drugname <- druglist[i]
        data[drugname] <- resultlist[[i]]
    }
    return(data)
}

#==============================================================================
# Co-prescription and other polydrug handling
#==============================================================================

cris$patient_ids_who_took <- function(patient_df, druginfo_df, drugname)
{
    cat("cris$patient_ids_who_took...\n")
    df <- sqldf(paste("
        SELECT DISTINCT(patient_df.id) AS took_id
        FROM patient_df, druginfo_df
        WHERE druginfo_df.id = patient_df.id
        AND druginfo_df.drugname = '", drugname, "'
    ", sep=""))
    return(df$took_id)

    #    AND druginfo_df.date >= patient_df.first_diagnosis_date
    #    AND druginfo_df.date <= patient_df.final_date
}

cris$drug_coprescription <- function(patient_df, druginfo_df, druglist)
{
    cat("cris$drug_coprescription...\n")
    n_drugs <- length(druglist)
    data <- matrix(NA, nrow=n_drugs, ncol=n_drugs)
    rownames(data) <- druglist
    colnames(data) <- druglist
    for (r in 1:n_drugs) {
        for (c in 1:n_drugs) {
            # LAYOUT: for row r, column c, the content is the number of patients, of those who took drug r, who took drug c
            r_drugname <- druglist[r]
            c_drugname <- druglist[c]
            cat("Processing:", r_drugname, "->", c_drugname, "\n")
            took_r <- cris$patient_ids_who_took(patient_df, druginfo_df,
                                                r_drugname)
            n_took_r <- length(took_r)
            if (n_took_r == 0 || r == c) {
                n_took_c_as_well <- n_took_r
            }
            else {
                took_c_as_well <- cris$patient_ids_who_took(
                    subset(patient_df, id %in% took_r),
                    druginfo_df,
                    c_drugname)
                n_took_c_as_well <- length(took_c_as_well)
            }
            data[r, c] <- n_took_c_as_well
        }
    }
    return(data)
}

cris$add_multidrug_column_count <- function(df, multidrug_column_name,
                                            individual_drug_columns)
{
    cat("cris$add_multidrug_column_count...\n")
    adply(
        df
        , 1 # by rows
        , function(x) {
            i <- x[individual_drug_columns] # individual drugs, probably as Booleans
            result <- sum(as.numeric(i))
            names(result) <- multidrug_column_name
            return(result)
        }
        , .parallel=TRUE
    )
}

cris$add_multidrug_column_any <- function(df, multidrug_column_name,
                                          individual_drug_columns)
{
    cat("cris$add_multidrug_column_any...\n")
    df <- cris$add_multidrug_column_count(df, multidrug_column_name,
                                          individual_drug_columns)
    df[multidrug_column_name] <- ifelse(df[multidrug_column_name] > 0,
                                        TRUE, FALSE)
    return(df)
}

cris$polydrug_details <- function(drugs_by_patient_period, drugnames,
                                  MONOTHERAPY_NAME="-") {
    # PARAMETERS
    #   drugs_by_patient_period
    #       which comes from: admissions_and_drug_use_by_calendar_periods
    #       COLUMNS:
    #           id: patient
    #           period_num: period
    #           ... drug names ...:
    #       ROWS:
    #       one per {patient, period_num} combination
    # RETURNS
    #   a list:
    #       results: data frame (drug1, drug2, simultaneous, ever)
    #       simultaneous }
    #       ever         } matrix versions for display
    #       combined     }

    # Use ldply twice to perform a loop within a loop, but with its
    # parallel-processing abilities.
    cat("cris$polydrug_details...\n")
    ldply(
        c(drugnames, MONOTHERAPY_NAME),  # using NA makes it harder!
        function(drug1) {  # from the outer ldply
            ldply(
                c(drugnames, MONOTHERAPY_NAME),
                function(drug2) {  # from the inner ldply
                    if (drug1 == MONOTHERAPY_NAME
                            && drug2 == MONOTHERAPY_NAME) {
                        # Nonsensical
                        simultaneous <- NA
                        ever <- NA
                    } else if (drug1 == MONOTHERAPY_NAME
                                || drug2 == MONOTHERAPY_NAME) {
                        # Monotherapy question
                        if (drug1 == MONOTHERAPY_NAME) {
                            d1 <- drug2  # swap
                            d2 <- MONOTHERAPY_NAME
                        } else {
                            d1 <- drug1  # keep
                            d2 <- MONOTHERAPY_NAME
                        }
                        # ... don't alter drug1/drug2, or the resulting data
                        # frame screws up! Use temporary variables.
                        on1 <- drugs_by_patient_period[, d1]
                        pts_ever_had_1 <- drugs_by_patient_period[on1, "id"]
                        all_drugs_except_1 <- drugnames[drugnames != d1]
                        on_any_except_1 <- apply(
                            drugs_by_patient_period[, all_drugs_except_1],
                            MARGIN=1,
                            FUN=function(x) { any(x) }
                        )
                        # Monotherapy at some point?
                        monotherapy <- on1 & !on_any_except_1
                        pts_ever_had_1 <- drugs_by_patient_period[on1, "id"]
                        pts_ever_had_another <- drugs_by_patient_period[
                            on_any_except_1, "id"]
                        # ... both contain repeats, but setdiff() removes them
                        # "ever" here is: Always monotherapy?
                        ever <- length(setdiff(pts_ever_had_1,
                                               pts_ever_had_another))
                        # "simultaneous" here is: monotherapy at some point?
                        simultaneous <- length(unique(
                            drugs_by_patient_period[monotherapy, "id"]
                        ))
                    } else {
                        # Co-prescription question
                        # How many patients who ever have drug1 also (ever
                        # or simultaneously) had drug2?
                        on1 <- drugs_by_patient_period[, drug1]
                        on2 <- drugs_by_patient_period[, drug2]
                        pts_ever_had_1 <- drugs_by_patient_period[on1, "id"]
                        pts_ever_had_2 <- drugs_by_patient_period[on2, "id"]
                        # ... both contain repeats, but intersect() removes them
                        ever <- length(intersect(pts_ever_had_1, pts_ever_had_2))
                        # How many patients on both at the same time?
                        simultaneous <- length(unique(
                            drugs_by_patient_period[on1 & on2, "id"]
                        ))
                    }
                    data.frame(
                        drug1=drug1,
                        drug2=drug2,
                        simultaneous=simultaneous,
                        ever=ever
                    )
                }
            )
        },
        .parallel=TRUE
    )
}

cris$format_polydrug_tables <- function(results, limit_to=NULL,
                                        drug_shortnames=NULL,
                                        drop_lower_triangle=FALSE,
                                        MONOTHERAPY_NAME="-",
                                        n_digits_pad=3)
{
    # PARAMETER
    #   results of cris$polydrug_details
    #   limit_to: list of drug names
    #   drug_shortnames: mapping c("longname1"="shortname1", ...)

    cat("cris$format_polydrug_tables...\n")

    process <- function(x) {
        if (drop_lower_triangle) {
            x[lower.tri(x)] <- ""
        }
        data.frame(x)
    }

    if (!is.null(limit_to)) {
        cat("... limiting \n")
        limit_to <- c(limit_to, MONOTHERAPY_NAME)
        results <- subset(results, drug1 %in% limit_to & drug2 %in% limit_to)
        results$drug1 <- factor(results$drug1, levels=limit_to)
        results$drug2 <- factor(results$drug2, levels=limit_to)
        # ... if you don't alter the levels, the dropped ones hang around
    }

    if (!is.null(drug_shortnames)) {
        cat("... renaming \n")
        results$drug1 <- revalue(results$drug1, drug_shortnames,
                                 warn_missing=FALSE)
        results$drug2 <- revalue(results$drug2, drug_shortnames,
                                 warn_missing=FALSE)
    }

    cat("... simultaneous\n")
    simultaneous <- process(daply(
        results, .(drug1, drug2),
        function(x) str_pad(x$simultaneous, n_digits_pad, pad=" "),
        .parallel=TRUE))
    cat("... ever\n")
    ever <- process(daply(
        results, .(drug1, drug2),
        function(x) str_pad(x$ever, n_digits_pad, pad=" "),
        .parallel=TRUE))
    cat("... combined\n")
    combined <- process(daply(
        results, .(drug1, drug2),
        function(x) {
            paste(str_pad(x$simultaneous, n_digits_pad, pad=" "),
                  ":",
                  str_pad(x$ever, n_digits_pad, pad=" ", side="right"),
                  sep="")
        },
        .parallel=TRUE))

    return(list(simultaneous=simultaneous,
                ever=ever,
                combined=combined))
}

#==============================================================================
# HoNOS
#==============================================================================

cris$cpft.get_honos <- function(dbhandle, idlist)
{
    # Note also: HoNOS_65, HoNOS_LD, HoNOS_SECURE, HoNOSCA
    # Beware with very long lists - will crash the SQL engine
    r <- sqlQuery(dbhandle, paste("
        SELECT
            document_id
            /* , clinician */
            , CAST(assessmentdate AS DATE) AS assessmentdate
            /* , totalscore AS totalscore_wrong */ /* this field adds 9s to the total but 9 means not known */
            , CASE periodrated WHEN 'xNx' THEN NULL ELSE periodrated END AS periodrated
            , CASE periodratedother WHEN 'xNx' THEN NULL ELSE periodratedother END AS periodratedother
            /* Numerical fields: from 'xNx' or e.g. '1 - minor problem requiring no action' or '9 - not known' */
            , CASE q1  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q1 , 1) AS INT) END AS q1
            , CASE q2  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q2 , 1) AS INT) END AS q2
            , CASE q3  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q3 , 1) AS INT) END AS q3
            , CASE q4  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q4 , 1) AS INT) END AS q4
            , CASE q5  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q5 , 1) AS INT) END AS q5
            , CASE q6  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q6 , 1) AS INT) END AS q6
            , CASE q7  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q7 , 1) AS INT) END AS q7
            , CASE q8  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q8 , 1) AS INT) END AS q8
            , CASE q8a WHEN 'xNx' THEN NULL ELSE q8a END AS q8a
            , CASE q9  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q9,  1) AS INT) END AS q9
            , CASE q10 WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q10, 1) AS INT) END AS q10
            , CASE q11 WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q11, 1) AS INT) END AS q11
            , CASE q12 WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q12, 1) AS INT) END AS q12
            , CASE q13 WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(q13, 1) AS INT) END AS q13
            , CASE qa  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(qa , 1) AS INT) END AS qa
            , CASE qb  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(qb , 1) AS INT) END AS qb
            , CASE qc  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(qc , 1) AS INT) END AS qc
            , CASE qd  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(qd , 1) AS INT) END AS qd
            , CASE qe  WHEN 'xNx' THEN NULL WHEN '9 - not known' THEN NULL ELSE CAST(LEFT(qe , 1) AS INT) END AS qe
            , CASE settingwhererated WHEN 'xNx' THEN NULL ELSE settingwhererated END AS settingwhererated
            , CASE settingwhereratedother WHEN 'xNx' THEN NULL ELSE settingwhereratedother END AS settingwhereratedother
            , CASE primarydiagnosis WHEN 'xNx' THEN NULL ELSE primarydiagnosis END AS primarydiagnosis
            , CASE primarydiagnosisother WHEN 'xNx' THEN NULL ELSE primarydiagnosisother END AS primarydiagnosisother
            , create_dttm
            , modif_dttm
            , brcid
        FROM cpft_endsql.dbo.honos
        WHERE ", cris$sql_id_in_list(idlist, "BrcId"), "
    ", sep=""))
    r <- within(r, {
        assessmentdate <- as.Date(assessmentdate)
        core_complete <- (
            !is.na(q1)
            & !is.na(q1)
            & !is.na(q2)
            & !is.na(q3)
            & !is.na(q4)
            & !is.na(q5)
            & !is.na(q6)
            & !is.na(q7)
            & !is.na(q8)
            & !is.na(q9)
            & !is.na(q10)
            & !is.na(q11)
            & !is.na(q12)
        )
        # Not sure what Q13 is! Nor QA-QE...
        total <- ifelse(core_complete,
                        q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9 + q10
                            + q11 + q12,
                        NA)
    })

    return(r)
}

#==============================================================================
# Drug history precision/recall
#==============================================================================

cris$cpft.clozapine_clinic_attendance_letters_subset <- function(dbhandle,
                                                                 idlist)
{
    query <- paste("
        SELECT
            brcid AS id
            , document_id
            , eventtime
        FROM cpft_endsql.dbo.DocumentLibrary
        WHERE ", cris$sql_id_in_list(idlist, "BrcId"), "
            AND CONTAINS(DocumentImage, ' \"clozapine review & monitoring sheet\" ')
    ", sep="")

    # double quotation marks within single quotes for phrases with CONTAINS:
    # http://support.microsoft.com/kb/246800

    # The phrase "Clozapine Review & Monitoring Sheet" is part of a template
    # used within the clozapine clinic.

    r <- sqlQuery(dbhandle, query)
    cat(query)
    r <- within(r, {
        eventtime <- as.Date(eventtime)
    })
    return(r)
}

cris$fetch_gate_source_row <- function(cn_doc_id, src_table, src_col,
                                       annotation_start, annotation_end,
                                       extrachars=200)
{
    # Works for single rows only

    phrase_start <- annotation_start
    phrase_len <- 1 + annotation_end - annotation_start

    before_start <- max(0, annotation_start - extrachars)
    before_len <- annotation_start - before_start

    after_start <- annotation_end + 1
    after_len <- extrachars

    query <- paste("
        SELECT
            SUBSTRING(", src_col, ",", before_start, ",", before_len, ") AS beforephrase
            , SUBSTRING(", src_col, ",", phrase_start, ",", phrase_len, ") AS phrase
            , SUBSTRING(", src_col, ",", after_start,  ",", after_len,  ") AS afterphrase
        FROM
            cpft_endsql.dbo.", src_table, "
        WHERE
            document_id = '", cn_doc_id, "'
    ", sep="")
    # cat(query)
    r <- sqlQuery(dbhandle, query)
    r <- within(r, {
        beforephrase <- as.character(beforephrase)
        phrase <- as.character(phrase)
        afterphrase <- as.character(afterphrase)
    })
    return(r)
}

cris$add_drug_context <- function(dbhandle, drughistorydf, extrachars=200)
{
    # drughistorydf: from get_gate_medication_current
    drughistorydf <- within(drughistorydf, { # columns get added in reverse order
        human_verified_correct <- NA
        afterphrase <- NA
        phrase <- NA
        beforephrase <- NA
    })
    n <- nrow(drughistorydf)
    for (i in 1:n) {
        cat("cris$add_drug_context: row", i, "/", n, "\n")
        src <- cris$fetch_gate_source_row(
            drughistorydf$cn_doc_id[i]
            , drughistorydf$src_table[i]
            , drughistorydf$src_col[i]
            , drughistorydf$annotation_start[i]
            , drughistorydf$annotation_end[i]
            , extrachars
        )
        drughistorydf$beforephrase[i] <- src$beforephrase[1]
        drughistorydf$phrase[i]       <- src$phrase[1]
        drughistorydf$afterphrase[i]  <- src$afterphrase[1]
    }
    return(drughistorydf)
}

cris$drug_context_for_drug <- function(dbhandle, drughistorydf, drug_name,
                                       extrachars=200)
{
    return(
        cris$add_drug_context(
            dbhandle
            , subset(drughistorydf, drugname == drug_name)
            , extrachars
        )
    )
}

cris$patients_who_took <- function(drughistorydf, drug_name)
{
    unique(subset(drughistorydf, drugname == drug_name)$id )
}

cris$manually_verify_drug_context <- function(contextdf, current=TRUE)
{
    if (current) {
        prompt <- "Enter 1 if drug was current, or 0 if wrong drug or wrong time, or NA to leave unchanged, or 99 to abort, or 55 to go back:"
    } else {
        prompt <- "Enter 1 if drug was taken at some point, or 0 if wrong drug, or NA to leave unchanged, or 99 to abort, or 55 to go back:"
    }
    n <- nrow(contextdf)
    i <- 1
    while (i <= n) {
        r <- contextdf[i,]
        cat("\n\n\n")
        cat("GATE THINKS:\n")
        cat("\n")
        cat("Date:", as.character(r$date), "\n")
        cat("Drug:", as.character(r$drug), "\n")
        cat("Dose:", as.character(r$dose), "\n")
        cat("Frequency:", as.character(r$frequency), "\n")
        cat("Time unit:", as.character(r$time_unit), "\n")
        cat("IGNORE - Tense:", as.character(r$tense), "\n")
        cat("\n")
        cat("SOURCE WAS:\n")
        cat("\n")
        cat(r$beforephrase, "\n")
        cat("\n")
        cat(r$phrase, "\n")
        cat("\n")
        cat(r$afterphrase, "\n")
        cat("\n\n\n", prompt)
        c <- scan(what=integer(), n=1)
        if (!is.na(c)) {
            if (c == 1) {
                cat("TRUE\n")
                contextdf[i,"human_verified_correct"] <- TRUE
            }
            else if (c == 0) {
                cat("FALSE\n")
                contextdf[i,"human_verified_correct"] <- FALSE
            }
            else if (c == 55 && i > 1) {
                cat("BACK ONE\n")
                i <- i - 2 # back one...
            }
            else if (c == 99) {
                cat("ABORT\n")
                return(contextdf)
            }
            else {
                cat("INVALID, TRY AGAIN\n")
                i <- i - 1 # do this one again
            }
        }
        i <- i + 1
    }
    return(contextdf)
}

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("cris" %in% search()) detach("cris")
attach(cris)  # subsequent additions not found, so attach at the end
