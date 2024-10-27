
##########################################################
##########################################################
##  PRELIMINARY ACTIONS 
##########################################################
##########################################################

# load required libraries
library(shiny)
library(shinyBS) # for bsCollapse and bsModal
library(superb)
library(ggplot2)
library(foreign) # for read.spss
library(stringr) # for str_remove_all and str_replace_all
library(dplyr)

appversion <- "App version 3.7; shipped with superb 0.95.19"

##########################################################
##########################################################
##  GENERIC FUNCTIONS 
##########################################################
##########################################################

is.something <- function( expr ) {
    if (is.null(expr)) return(FALSE) 
    else if (length(expr) == 0) return(FALSE)
    else if (all(((class(expr) == "character")&(expr == "")))) return(FALSE)
    else return(TRUE)    
}


# for nicer output p(em(...)) and code(...)+br()
bc <- function(...) {tagList(code(...), br())}
pem <- function(...) {
    if (length(strsplit(..., "\n")[[1]]) >1) {
        lapply(strsplit(..., "\n")[[1]], bc)
    } else {
        p(em(...))
    }
}


# to get only the digits and convert to integer; to get numerics including Inf, and to get tokens
to.integer <- function(str) { #only positive integers
    dgt <- regmatches(str, regexpr("[[:digit:]]+", str))
    as.integer(ifelse(length(dgt)>0,dgt,"0"))
}
to.numeric <- function(str) {
    dgt <- regmatches(str, regexpr("[+-]?[[:digit:].]+|[+-]?Inf", str))
    if (length(dgt)>0) {
        if ((dgt == "+Inf")||(dgt == "Inf")) Inf else if (dgt == "-Inf") -Inf else
        as.numeric(dgt)
    } else {
        0
    }
}
to.identifier <- function(str) {
    lts <- regmatches(str, regexpr("[A-Za-z][A-Za-z0-9.]*", str))
    ifelse(length(lts)>0,lts,"MISSING_NAME")
}


# ignores NA in pasting 
# https://stackoverflow.com/questions/13673894/suppress-nas-in-paste
paste3 <- function( ... ) {
    tmp <- Filter(length, lapply(list(...), function(x) {return(x[!is.na(x)])}))
    do.call("paste", tmp)
}


# a function to navigate through a shiny object to alter its content
# https://stackoverflow.com/questions/51611865/edit-a-shiny-tag-element/67863101#67863101
searchreplaceit <- function(branch, whattag, whatattribs, totag, toattribs, replace=TRUE) {
    if ("name" %in% names(branch)) {
        if ((branch$name == whattag)&(identical( branch$attribs[names(whatattribs)], whatattribs))) {
            branch$name    <- totag
            branch$attribs <- if (replace) {toattribs} else { modifyList(branch$attribs, toattribs)}
        }
    }
    if ("shiny.tag" %in% class(branch)) {
        if (length(branch$children)>0) for (i in 1: length(branch$children)) {
            if (!(is.null(branch$children[[i]]))) {
                branch$children[[i]] = searchreplaceit(branch$children[[i]], whattag, whatattribs, totag, toattribs, replace)
        } }
    } else if ("list" %in% class(branch)) {
        if (length(branch)>0) for (i in 1:length(branch) ) {
            if (!(is.null(branch[[i]]))) {
                branch[[i]] <- searchreplaceit(branch[[i]], whattag, whatattribs, totag, toattribs, replace)
        } }
    } 
    return(branch)
}


# capture all the messages produced by a function call; simplified from
# https://www.r-bloggers.com/2020/10/capture-message-warnings-and-errors-from-a-r-function/
cLogs <- function(f, ...) {
    logs     <- data.frame(type = character(0), content = character(0) )
    addToLog <- function(type, message) {
        oneline <- data.frame(type = type, content =  message)
        logs <<- rbind(logs, oneline)
    }
    res <- withCallingHandlers(
        tryCatch(do.call(f, ...), 
            error   = function(e) {
                addToLog("error", str_remove_all(conditionMessage(e), "[\r\n]$"))
                NULL
            }), 
        warning = function(w) {
            addToLog("warning", str_remove_all(conditionMessage(w), "[\r\n]$"))
            invokeRestart("muffleWarning")
        }, 
        message = function(m) {
            addToLog("message", str_remove_all(conditionMessage(m), "[\r\n]$"))
            invokeRestart("muffleMessage")
        }
    )
    return(list(res = res, logs = logs))
}


##########################################################
##########################################################
## THE USER INTERFACE UI
##########################################################
##########################################################

# the help windows.
theHelpModals <- list(
    bsModal(id=301, title = "Help on Step 1", trigger = "S1More", 
        p("Step 1 loads a valid data file."),
        p("To be valid, the file must:"),
        tags$ul(
            tags$li("be a .csv (",em("comma-separated values"),"), .tsv (",em("tab-separated values"),") or .sav (",em("spss"),") file;"),
            tags$li("(for .csv and .tsv) have on the very first line the name of the columns;"),
            tags$li("contain on the remaining lines the data in wide format")),
        p(strong("Wide format")," implies that there is one line per subject. ",
            "If the subjects have been measured multiple times, there is one column per measure.")
    ),
    bsModal(id=302, title = "Help on Step 2", trigger = "S2More", 
        p("Step 2 speficies the experimental design of the data."),
        p("The data can involve between-subject factors. The ''grouping'' of",
            "the participants is signaled by group columns. There can be one or",
            "many between-subject factors, whose levels are contained in columns (one per between-subject factor)."),
        p("The data can also involve within-subject factors. In a wide-format data, ",
            "the within-subject factors (often refered to as repeated-measures) are to be ",
            "found in multiple `Variable` columns. The name of the within-subject factors must be provided, ",
            "as well as the number of levels of the factors. When more than one within-subject factors are present, ",
            "the design is either full-factorial with the total number of levels (i.e., ",
            "the total number of columns) given by the product of the within-subject factor levels ",
            "; or the design is not full-factorial, in which case, for each variable, it is necessary to ",
            "indicate the factor level for each factor. A modal window will show up when this is the case.  "
        )
    ),
    bsModal(id=303, title = "Help on Step 3", trigger = "S3More", 
        p("Step 3 selects the statistics (point estimates and intervals) to be illustrated."),
        p("The point estimates are the statistics to be displayed. The usual is to display the mean ",
            "or the median, but other summary statistics can be selected."),
        p("The intervals are precision estimates used to set the confidence interval limits. ",
            "Precision estimates are commonly confidence intervals (CI) but can also be standard error (SE). ",
            "Either of these can be estimated with analytical formulas or from bootstrap estimates."
        )
    ),
    bsModal(id=304, title = "Help on Step 4", trigger = "S4More", 
        p("Step 4 selects the adjustments to the error bars."),
        p(strong("Purpose: "),"Error bars can be to compare the plotted value to a fixed point.",
            "Such error bars are also called ",em("stand-alone"), ". ", em("Pairwise")," error bars",
            ", also called ",em("difference-adjusted"), " error bars are used to perform pairwise",
            "comparisons. Finally, ", em("Tryon"),"-adjusted error bars are also for pairwise",
            "comparisons except that when they are of difference length, Tryon-adjusted error bars",
            "can be averaged using the standard average."),
        p(strong("Decorrelation: "),"(excluding stand-alone adjustments) are techniques that allow",
            "comparing repeated measures obtained from a within-subject design. Techniques includes", 
            em("Loftus and Masson, 1994,"), "in which all the standard errors are pooled to yield a unique", 
            "error bar length;", em("Cousineau-Morey, 2005, 2008 (as per Baguley, 2012)"), "keeps the ",
            "error bar length unpooled (and possibly of different length). Finally, the ", em("correlation adjustments (Cousineau, 2019)"),
            "is based on the average correlation instead of a transformation of the data matrix.",
            "All three techniques are fairly similar and are estimators of the same quantity."
        ),
        p(strong("Population size adjustment: ")," When a sizeable proportion of the whole population",
            "is in the sample, precision is improved. When the population is large, it is said to be",
            em("infinite"), ", noted in R with ", em("Inf"))
    ),
    bsModal(id=305, title = "Help on Step 5", trigger = "S5More", size="large",
        p("Step 5 chooses a layout and adds directives to decorate the plots."),
        p(strong("Order of the factors."),"The first factor is placed on the horizontal axis,",
            "the second is used to make multiple lines/bars, the third factors makes for ",
            "multiple panels on a row, and if there is a fourth factors, it generates multiple ",
            "rows of plots. Using `Select the order ...`, you can change the variables order ",
            "but  you must place them all."
        ),
        p(strong("Plot's layout."),"The default layout shows the data using bars. You can select",
            "among other layouts, to obtain lines, (or just points), along with indications of ",
            "the raw data (pointjitter and pointjitterviolin). The pointjitterviolin and the",
            "raincloud layouts additionally shows a violin estimating the distribution."
        ),
        p(strong("Specific graphic attributes."),"You can add additional attributes to specific ",
            "plot's elements. For example, errorbarParams will inject additional attributes to the",
            "error bars. The attributes must be comma-separated. Here are some examples.",br(),
        ),
        p(em("errorbarParams:")),
            tags$table(
                tags$tr(tags$td("shifts the error bars to the left"),tags$td(code("position = position_dodge(width = .15)"))),
                tags$tr(tags$td("makes them wider, thicker or gray"),tags$td(code("width = .2, size = 3, colour = \"gray\""))),
                tags$tr(tags$td("triple the tips"),tags$td(code("tipformat = \"triple\", tipgap = 0.4, direction = \"left\"")))
            ),

        br(),p(em("barParams:")),
            tags$table(
                tags$tr(tags$td("change line type, color, thickness"),tags$td(code("linetype = 3, colour = \"black\", size = .5"))),
            ),

        br(),p(em("pointParams:")),
            tags$table(
                tags$tr(tags$td("moves the points away"),tags$td(code("position = position_dodge(width = .15)"))),
                tags$tr(tags$td("change their color and the size"),tags$td(code("colour = \"gray\", size = 10.5")))
            ),

        br(),p(em("lineParams")),
            tags$table(
                tags$tr(tags$td("change line thickness and line style"),tags$td(code("size=0.25, linetype=\"dashed\" ")))
            ),

        br(),p(em("jitterParams")),
            tags$table(
                tags$tr(tags$td("change the size of the individual dots"),tags$td(code("size = 0.5"))),
                tags$tr(tags$td("shapes above 20 have fillings"),tags$td(code("alpha=1, shape=21, fill=\"white\"")))
            ),
            
        br(),p(em("violinParams")),
            tags$table(
                tags$tr(tags$td("set the transparency and the color of the filling"),tags$td(code("alpha =0.7, fill = \"green\" ")))
            ),

        br(),p(strong("General graphic directives"), "Provides graphic-wide directives that",
            "will affect the whole figure. The directives must be one per line; you can ",
            "comment some of these with #. Examples are: "),
            tags$table(
                tags$tr(tags$td("flip the plot sideways; nicer for rainplot"),tags$td(code("coord_flip( ylim = c(50,100) )"))),
                tags$tr(tags$td("adds a title"),tags$td(code("labs(title =\"Main title\")"))),
                tags$tr(tags$td("adds a label on the x axis"),tags$td(code("xlab(\"Moment\")       "))),
                tags$tr(tags$td("adds a label on the y-axis"),tags$td(code("ylab(\"Score\")        ")),),
                tags$tr(tags$td("restricts the vertical range"),tags$td(code("coord_cartesian( ylim = c(50,100) ) ")),),
                tags$tr(tags$td("show **"),tags$td(code("showSignificance( c(1, 3), 90, -1, \"**\")")),),
                tags$tr(tags$td("change fill colors"),tags$td(code(" scale_fill_manual( name = \"Group\", labels = c(\"A\", \"B\"), ",
                    "values = c(\"blue\", \"purple\"))")))
            ),
        p("Check ggplot2 documentations regarding these attributes and directives.")
    )
)

theExtraModals <- list(
    # hack the modal so that it has easyClose = FALSE
    tlist <- searchreplaceit(
        # hack the modal so that the Close button has an id to be observed upon
        searchreplaceit(
            bsModal(id="superbnonfact", title = "Non-factorial design", trigger = "test",
                uiOutput("superbNonFactorialDesign")
            ),
            "button", list("class"="btn btn-default"),
            "button", list("class"="btn btn-default action-button", "id"="nonfactclose", "data-dismiss"="modal"), 
            replace=TRUE
        ),
        "div", list("class"="modal sbs-modal fade"),
        "div", list("data-backdrop"="static", "data-keyboard"="false"),
        replace=FALSE
    )
)


# make the page, aka user interface (ui)
thePage <- fluidPage(
    titlePanel("", windowTitle = "Summary plots with adjusted error bars"),
    tags$head(tags$link(rel = "shortcut icon", type="image/x-icon", href="https://dcousin3.github.io/superb/logo.png")),
    tags$head(tags$style(HTML("input[type='text']:invalid {background-color: pink;}"))),

    # load the modals
    theHelpModals,
    theExtraModals,

    # set the layout of the page
    sidebarLayout(
        sidebarPanel(
            # putting logo here
            h4(
                img(src="https://dcousin3.github.io/superb/logo.png",
                    alt="logo", 
                    style="height:64px; width:64px; float:left; margin-right:5px; margin-top:-7px;"
                ),
                p("Summary plots with adjusted error bars"),
                a("dcousin3.github.io/superb", href="https://dcousin3.github.io/superb", target="_blank", style="font-size: 10px;")
            ),
            # collapsible panels
            bsCollapse(id = "collapseExample", open = 1,
                bsCollapsePanel("Step 1: Load the data", 
                    p(strong("Select the data file")),
                    fileInput("superbFile", NULL, accept=".csv,.tsv,.sav"),
                    bsButton("S1Prev", "Previous", disabled = TRUE),
                    bsButton("S1Apply", "Apply", disabled = TRUE),
                    bsButton("S1Next", "Next", disabled = TRUE),
                    bsButton("S1More","", icon=icon("question-circle")), #help(icon)
                    value = 1, style = "success"),
                bsCollapsePanel("Step 2: Specify the experimental design", 
                    selectInput("superbBSFactors", "Select between-subject factors",   choices = NULL, multiple = TRUE),
                    selectInput("superbVariables", "Select the dependent variable(s)", choices = NULL, multiple = TRUE),
                    uiOutput("superbWSFactors1"),   # conditional on the number of variables...
                    uiOutput("superbWSFactors2"),
                    uiOutput("superbWSFactors3"),
                    uiOutput("superbWSFactors4"),
                    bsButton("S2Prev", "Previous"),
                    bsButton("S2Apply", "Apply", disabled = TRUE),
                    bsButton("S2Next", "Next", disabled = TRUE ),
                    bsButton("S2More","", icon=icon("question-circle")),
                    value = 2, style = "success"),
                bsCollapsePanel("Step 3: Select summary and error bar statistics", 
                    selectInput("superbStatistic", "Select summary statistics", choices=c("mean","median","hmean","gmean","var","sd","MAD","IQR","fisherskew","pearsonskew","fisherkurtosis","meanNArm"), multiple=FALSE),
                    div(style="display:inline-block", 
                        selectInput("superbErrorbar",  "Select error bar function", choices=c("CI","SE","bootstrapSE","bootstrapPI"), multiple = FALSE, width="200px", selectize =FALSE)
                    ),
                    div(style="display:inline-block", 
                        uiOutput("superbGamma")
                    ), br(),
                    bsButton("S3Prev", "Previous"),
                    bsButton("S3Apply", "Apply", disabled = TRUE),
                    bsButton("S3Next", "Next", disabled = TRUE),
                    bsButton("S3More","", icon=icon("question-circle")),
                    value = 3, style = "success"),
                bsCollapsePanel("Step 4: Select adjustments", 
                    radioButtons("superbPurpose", "Select the objective of the error bars",
                        choiceNames  = c("Stand-alone","for pairwise comparisons","Tryon (2001) adjustment"),
                        choiceValues = c("single","difference","tryon"), 
                        selected="single"
                    ),
                    uiOutput("S4Decorrelation"),
                    textInput("superbPopsize", "Select the size of the population",
                        value = "Inf", placeholder = "Inf for infinite or an integer number"),
                    bsButton("S4Prev", "Previous"),
                    bsButton("S4Apply", "Apply", disabled = FALSE),
                    bsButton("S4Next", "Next", disabled = TRUE),
                    bsButton("S4More","", icon=icon("question-circle")), 
                    value = 4, style = "success"),
                bsCollapsePanel("Step 5: Select layout and ornaments", 
                    selectInput("superbPlotorder", "Select the order in which factors are plotted", 
                        multiple = TRUE, choices=c(""), selected="" ),
                    selectInput("superbLayout", "Select the plots' layout",
                        choices=c("bar","point","line",
                                  "pointjitter","pointlinejitter",
                                  "pointjitterviolin","pointindividualline",
                                  "raincloud","halfwidthline","boxplot","lineBand","corset",
                                  "circularpoint","circularline","circularpointjitter",
                                  "circularpointlinejitter","circularlineBand"),
                        multiple = FALSE, selectize = FALSE),
                    strong("Specific graphic attributes (comma separated)"),br(),
                    div(style="display:inline-block", # if one choice is named, they must all be?
                        selectInput("directive1", NULL, choices=c("Choose one" = "", "errorbarParams"="errorbarParams","facetParams"="facetParams","barParams"="barParams","pointParams"="pointParams","lineParams"="lineParams","jitterParams"="jitterParams","violinParams"="violinParams","errorbandParams"="errorbandParams"), 
                            width="140px", selectize = FALSE )
                    ),
                    div(style="display:inline-block", 
                        textInput("content1", NULL, width="200px"), 
                    ),
                    div(style="display:inline-block", bsButton("delete1","",icon=icon("trash-alt"),size="small")),
                    uiOutput("linedir2"),
                    uiOutput("linedir3"),
                    uiOutput("linedir4"),
                    uiOutput("linedir5"),
                    textAreaInput("ornates", "General graphic directives (one per line)", resize = "vertical"),
                    bsButton("S5Prev", "Previous"),
                    bsButton("S5Apply", "Apply", disabled = FALSE),
                    bsButton("S5Next", "Next", disabled = TRUE),
                    bsButton("S5More","", icon=icon("question-circle")), 
                    value = 5, style = "success"),
                bsCollapsePanel("All done!", 
                    p(em("Thank you for using superb.")),
                    p("To cite this work, ", a("doi: 10.1177/25152459211035109", href="https://doi.org/10.1177/25152459211035109", target="_blank"),"."),
                    p("For issues, ", a("github.com/dcousin3/superb/issues", href="https://github.com/dcousin3/superb/issues", target="_blank"),"."),
                    p("Tip: Cut-and-paste the script generated (last tab) for", 
                      "easier re-run of the instructions and advanced customization."),
                    p("If you are in R, you can interrupt the app (with esc)",
                      "and type quit() to leave."),
                    bsButton("S6Prev", "Previous"),
                    value = 6, style = "success")
            ),
			p( appversion, style="font-size: 10px;" )
        ),
        mainPanel(
            tabsetPanel( id = "MainDisplay",
                tabPanel("Messages", 
                    uiOutput(outputId="superbMessages1"),
                    uiOutput(outputId="superbMessages2"),
                    uiOutput(outputId="superbMessages3"),
                    uiOutput(outputId="superbMessages4")
                ),
                tabPanel("Summary plot", 
                    plotOutput("superbPlot", inline=TRUE, height=input$height, width=input$width), 
                    uiOutput("superbPlotCaption"),hr(),
                    bsCollapse(id="asdf", open=0,
                        bsCollapsePanel("Click to set plot dimensions", value=1, style="success",
                            p(strong("Dimensions of the plot")),
                            div(style="display:inline-block", sliderInput(inputId = "height", post=" px",
                                label = "height", min = 150, max = 2000, step = 5, value = 400)),
                            div(style="display:inline-block", sliderInput(inputId = "width", post=" px",
                                label = "width",  min = 150, max = 2000, step = 5, value = 600)),
                            p(strong("Use right-click to cut and paste the figure"))
                        )
                    )
                ),
                tabPanel("Summary data", uiOutput("superbDataCaption"), tableOutput("superbData")),
                tabPanel("Script generated", verbatimTextOutput(outputId="superbScript"))
            )
        )
    )
)

# remove the hyperlink on the collapsible tabs so that it is not possible to navigate freely
thePage <- searchreplaceit(thePage,
    "a", list("data-toggle" = "collapse", "data-parent"="#collapseExample"),
    "p", list(), replace=TRUE )



##########################################################
##########################################################
## THE SERVER LOGIC
##########################################################
##########################################################


# for entering within-subject factor names and levels with added pattern validation
wslinefct <- function( i ) {
    wsline = list(
        div(style="display:inline-block", textInput(paste("wsfact",i,sep=""), NULL, width = "210px", placeholder = paste("Within-subject factor",i,"name")) ),
        div(style="display:inline-block", textInput(paste("wsleve",i,sep=""), NULL, width = "60px", placeholder = "level" ) )
    )
    wsline <- searchreplaceit(wsline, "input", list(id=paste("wsfact",i,sep="")), 
            "input", list(pattern="[A-Za-z][A-Za-z0-9.]*"), replace=FALSE)
    wsline <- searchreplaceit(wsline, "input", list(id=paste("wsleve",i,sep="")), 
            "input", list(pattern="[0-9]*"), replace=FALSE)
    return(wsline)
}


# for entering graphic attributes
galinefct <- function( i ) {
    galine = list(
        div(style="display:inline-block", 
            selectInput(paste("directive",i,sep=""), NULL, choices=c("Choose one" = "", "errorbarParams"="errorbarParams","facetParams"="facetParams","barParams"="barParams","pointParams"="pointParams","lineParams"="lineParams","jitterParams"="jitterParams","violinParams"="violinParams","errorbarlightParams"="errorbarlightParams"), 
            width="140px", selectize = FALSE )),
        div(style="display:inline-block", textInput(paste("content",i,sep=""), NULL, width="200px")),
        div(style="display:inline-block", bsButton(paste("delete",i,sep=""),"",icon=icon("trash-alt"),size="small"))
    )
    return(galine)
}


# to enter factor levels in non-factorial design
## TO FINISH
makeNonfactorialContent <- function(output, varnames, wsfactornames, wsfactorlevels) {
    tlist = tagList(
        p("You have defined within-subject factor(s) ", 
            strong(paste(wsfactornames, collapse=", ")),
            " with ", paste(wsfactorlevels, collapse = " x "), 
            " levels", 
            ifelse(length(wsfactornames) >1, 
                paste(" totalizing ", prod(wsfactorlevels), ".", sep=""),"."),
            "However, only ", length(varnames), " dependent variables are listed."),
        p("Please specify for each variable(s) to what levels of the within-subject factor(s) it belongs:"),
        # make a table with factor names as columns and var names as line
        tags$table(
            tags$thead(
                tags$th( lapply( wsfactornames, tags$td, 
                                style="border-bottom: 1px solid #000;text-align: center;"
                        )  )
            ),
            tags$tbody(
                lapply(varnames, function(onevarname) {
                    tags$tr(tags$td(onevarname), 
                        lapply( wsfactornames, function(onewsname) {
                                # hack the shiny code to adjust manually height of cells
                                searchreplaceit(tags$td(textInput(paste("wscell",onevarname,onewsname,sep=""),
                                                        label =NULL, width="50px") ),
                                    "div", list("class"="form-group shiny-input-container"),
                                    "div", list("style"="width:50px;height=26"), replace=TRUE 
                                )
                            }
                        )
                    )
                })
            )
        ),
        p("Click \"Close\" to resume Step 2")
    )
    output$superbNonFactorialDesign  <- renderUI({ tlist })
}


####################################################################


# takes as input the collected information (herein cI) over the various steps
# and returns the script that could be run in terminal.
generateScript <- function( cI ) {
    # indentation for nicer output
    indent1 = paste(rep(" ", 4), collapse = "")
    indent2 = paste(rep(" ", 8), collapse = "")

    script = rep(NA, 6) # six empty strings, one per step
    if (cI$Completed < 1) return( script )

    # Step 0==script[1]: i.e., loading libraries
    topline <- "# Step 0: Load relevant libraries"
    topline <- paste(topline, "library(superb)      # main package", sep="\n")
    if(cI$Step1$ext=="sav") 
        topline <- paste(topline, "library(foreign)     # to read .sav spss files", sep="\n")
    if ((length(cI$Step5)>0)||(length(cI$Step6)>0)) {
        topline <- paste(topline, "library(ggplot2)     # for graphics ornaments", sep="\n")
    }

    # if unstandard extension tsv or sav:
    extline <- if(cI$Step1$ext=="tsv") {
        paste("# define a read.tsv function, as it is not in base R",
              "read.tsv <- function(...) {read.table( ..., sep=\"\\t\") }", sep="\n")
    } else if(cI$Step1$ext=="sav") {
        paste("# define a read.sav function for concision",
              "read.sav <- function(...) {read.spss( ..., to.data.frame=TRUE) }", sep="\n")
    } else {NA}
    # we're done with Step 0==script[1] information
    script[1] <- paste3( topline, extline, sep = "\n")

    # Step 1==script[2]: Load the data
    script[2] <- paste3( 
        "# Step 1: Load the data (adjust working directory if needed)",
        paste("dataToPlot <- read.",cI$Step1$ext,"(\"", cI$Step1$name, "\", header = TRUE)", sep=""),
        sep = "\n"
    )
    if (cI$Completed <=1) return( script )

    # Step 2==script[3]: Specify the experimental design
    bsfactline <- if (length(cI$Step2$BSFactors)==1) {
        paste(indent1, "BSFactors   = \"", cI$Step2$BSFactors, "\", ", sep="")
    } else if (length(cI$Step2$BSFactors) > 1) {
        paste(indent1, "BSFactors   = c(\"",paste(cI$Step2$BSFactors, collapse="\", \""), "\"), ", sep="") 
    } else {NA} # no between-subject factors

    wsfactline <- if (length(cI$Step2$WSFactors)==1) {
        paste(indent1, "WSFactors   = \"", cI$Step2$WSFactors, "\", ", sep="")
    } else if (length(cI$Step2$WSFactors) > 1) {
        paste(indent1, "WSFactors   = c(\"", paste(cI$Step2$WSFactors, collapse="\", \""), "\"),", sep="")
    } else {NA} #no within-subject factors

    wsnonfactline <- if(is.list(cI$Step2$WSDesign)) {
        paste(indent1, "WSDesign    = list(", 
            paste(unlist(lapply(cI$Step2$WSDesign, function(vec) paste("c(", paste(vec, collapse=", "),")", 
                                                    sep=""))),
                collapse=", "),
            "),", 
            sep="")
    } else {NA}

    varsline <- if (length(cI$Step2$variables)==1) {
        paste(indent1, "variables   = \"", cI$Step2$variables, "\",", sep="")
    } else if (length(cI$Step2$variables) > 1) {
        paste(indent1, "variables   = c(\"",paste(cI$Step2$variables, collapse="\", \""), "\"),", sep="") 
    } 

    statline <- paste(indent1, "statistic   = \"", cI$Step2$statistic, "\",", sep="")
    ebarline <- paste(indent1, "errorbar    = \"",  cI$Step2$errorbar, "\",", sep="")
    gammline <- if (is.something(cI$Step2$gamma)) {
        paste(indent1, "gamma       = ",  cI$Step2$gamma, ",", sep="") 
    } else {NA}

    script[3] <- paste3( 
        "# Step 2: Specify the experimental design",
        "superbPlot(dataToPlot,",
        bsfactline, 
        wsfactline,
        wsnonfactline,
        varsline,
        statline,
        ebarline,
        gammline,
        sep = "\n"
    )

    # Step 4==script[3]bis: Select adjustments
    purpose       <- paste(indent2, "purpose       = \"", cI$Step4$purpose, "\"", sep="")
    decorrelation <- if (is.something(cI$Step4$decorrelation)) {
        if (cI$Step4$decorrelation != "none")  {
            paste(indent2, "decorrelation = \"", cI$Step4$decorrelation, "\"", sep="")
        }
    } else {NA}
    popsize       <- if (is.something(cI$Step4$popSize)) {
        if (cI$Step4$popSize != Inf) 
            paste(indent2, "popSize       = ",   format(cI$Step4$popSize,scientific=FALSE), sep="")
    } else {NA}

    script[3] <- paste3(
        script[3],
        paste(indent1, "adjustments = list(",sep=""),
        paste3(purpose, decorrelation, popsize, sep=",\n"),
        paste(indent1, ")", sep=""),
        sep = "\n"
    )

    # Step 5==script[3] ter: for plotStyle and factorOrder
    plotlayout <- if (is.something(cI$Step2$plotStyle))
        paste(indent1, "plotStyle = \"", cI$Step2$plotStyle, "\"", sep="")
    else NA
    factorord <-  if (is.something(cI$Step2$factorOrder))
        paste(indent1, "factorOrder = c(\"", paste(cI$Step2$factorOrder,collapse="\", \""), "\")", sep="")
    else NA
    script[3] <- paste3(
        script[3], 
        paste3(plotlayout, factorord, sep=",\n"),
        sep = ",\n"
    )

    if (cI$Completed <=4 ) {
        script[3] <- paste(script[3],")", sep="\n")
        return( script )
    }

    # Step 5==script[3]quatro and script[4]: ggplots directives
    if (length(cI$Step5)!=0) {
        res = character(0)
        for (ga in names(cI$Step5)) 
            res = append(res, paste(indent1, ga, " = list(", cI$Step5[[ga]],")", sep=""))
        script[3] <- paste3(script[3], paste3(res, collapse=",\n"), sep=",\n")
    }

    script[3] <- paste(script[3],")", sep="\n")

    if (is.something(cI$Step6)) {
        script[3] <- paste(script[3]," + ", sep="")
        toto = str_remove_all(cI$Step6, "^[\r\n]|[\r\n]$")
        script[4] <- paste(
            "# additional graphic directives",
            str_replace_all(toto, "\n", " + \n"),
            sep="\n"
        )
    }

    return( script )
}


runAndShowIt <- function( input, output, currentInfo) {

    # arrange the parameters
    params <- currentInfo$Step2             # excluding graphic directives and attributes
    params$adjustments <- currentInfo$Step4 # the adjustments are in a sub-list
    gattrib <- currentInfo$Step5            # the graphic attributes
    gdirect <- currentInfo$Step6            # the graphic directives

    # in Step5, all the graphic attributes must be wrapped into lists...
    if (length(gattrib)!=0) {
        for (ga in names(gattrib)) {
            test = cLogs(eval, list(str2lang(paste("list(",gattrib[[ga]],")"))))
            errorsE <- test$logs[test$logs$type == "error",  ]$content
            if (length(errorsE)==0) errorsE <- NULL

            if (is.null(errorsE)) {
                params[[ga]] <- test$res
                output$superbMessages4 <- renderUI({ NULL })
            } else {
                output$superbMessages4 <- renderUI(
                    tagList( h4("Step 5: Errors were raised by ggplot attribute(s)..."),
                        lapply(errorsE, pem)
                ))
            }
        }
    }

    # run superbPlot with result as a plot
    resA <- cLogs(superbPlot, params)
    # run superbPlot with results as tables
    resB <- cLogs(superbPlot, modifyList(params, list(showPlot=FALSE)))

    #if Step6 then
    if (!is.null(currentInfo$Step6)) {
        # listify the ornates
        toto <- str_remove_all(currentInfo$Step6, "^[\r\n]|[\r\n]$")
        toto <- str_replace_all(toto,"\n",",\n")
            
        resC    <- cLogs(eval, list(str2lang(paste("list(",toto,"\n)"))))
        errorsC <- resC$logs[resC$logs$type == "error",  ]$content
        if (length(errorsC)==0) errorsC <- NULL

        if (is.null(errorsC)) {
            # use the ggplot's + operator
            resD <- cLogs(ggplot2:::`+.gg`, list( resA$res, resC$res ) )

            errorsD <- resD$logs[resD$logs$type == "error",  ]$content
            if (length(errorsD)==0) errorsD <- NULL

            if (!is.null(errorsD)) { # not good!
                output$superbMessages3 <- renderUI(
                    tagList( h4("Step 5: Errors were raised by ggplot directives..."),
                        lapply(errorsD, pem)
                    ))
            } else {
                # keep the plot with the graphic directives
                resA$res= resD$res 
                # remove message3 errors if any
                output$superbMessages3 <- renderUI({ NULL })
            }  

        } else {
            output$superbMessages3 <- renderUI(
                tagList( h4("Step 5: Errors were raised while interpreting ggplot directives..."),
                    lapply(errorsC, pem)
                ))
        }
    }

    # extract errors, warnings, and messages
    errorsA <- resA$logs[resA$logs$type == "error",  ]$content
    warninA <- resA$logs[resA$logs$type == "warning",]$content
    messagA <- resA$logs[resA$logs$type == "message",]$content
    if (length(errorsA)==0) errorsA <- NULL
    if (length(warninA)==0) warninA <- NULL
    if (length(messagA)==0) messagA <- NULL

    if (!is.null(errorsA)) { # not good!
        output$superbMessages2 <- renderUI(
            tagList( h4("Step 2: Errors were raised..."),
                lapply(errorsA, pem)
            ))
    } else if (!is.null(warninA)) { # not excellent!
        output$superbMessages2 <- renderUI(
            tagList( h4("Step 2: Warnings were raised..."),
                lapply(warninA, pem)
            ))
    } else if (!is.null(messagA)) { # better
        output$superbMessages2 <- renderUI(
            tagList( h4("Step 2: Messages were generated. No need to worry, this is for your information."),
                lapply(messagA, pem),
                p("Done. The preliminary plot is available in the tab",em("Summary plot")," and summary statistics are available under the tab ",em("Summary data"),".")
            ))
    } else { # all is good
        output$superbMessages2 <- renderUI(
            tagList( h4("Step 2: Speficying the experimental data."),
                p("Done.")
            ))
    }

    # make a figure caption adapted to the information:
    theCaption = generateCaption( currentInfo )

    # put the plot and the summary data in their respective tabs
    if (is.null(errorsA)) {
        output$superbPlotCaption <- renderUI(h4( theCaption ))
        output$superbPlot <- renderPlot(resA$res, height = input$height, width = input$width )
        output$superbDataCaption <- renderUI({h4("Table: Summary statistic (",em("center"),") and limits of the error bars (",em("lowerwidth"), " and ", em("upperwidth"),")")})
        output$superbData <- renderTable({resB$res$summaryStatistics})
    } else { # erase them
        output$superbPlot <- renderUI({ })
        output$superbPlot <- renderPlot({ })
        output$superbDataCaption <- renderUI({ })
        output$superbData <- renderTable({ })
    }

}


generateCaption <- function( currentInfo ) {
    paste("Figure.",
        if( currentInfo$Completed >=2 ) {
            f1 <- currentInfo$Step2$statistic
            f2 <- currentInfo$Step2$errorbar
            g  <- currentInfo$Step2$gamma
            paste3( f1, "with error bars showing", 
                if (currentInfo$Step4$purpose != "single") {
                    paste3(
                        if (currentInfo$Step4$decorrelation == "LM") {"pooled "} else {NA},

                        if (currentInfo$Step4$purpose == "difference") {"difference-"}
                        else if (currentInfo$Step4$purpose == "tryon") {"Tryon-"} 
                        else {NA},

                        if (substr(currentInfo$Step4$decorrelation,1,2) %in% c("LM","CM","CA","LD")) {" and correlation-"} else {NA},
                        "adjusted",
                        sep = "")
                } else {NA},
                if(superb:::is.gamma.required(paste(f2,f1,sep="."))) {
                    paste(100* to.numeric(g),"%", sep="")
                },
                f2, "of the", f1, 
                if (currentInfo$Step4$popSize != Inf) {"adjusted for finite population size"} else {NA}
            )

        } else { "mean with error bars showing 95% CI of the mean" },
        sep = " "
    )
}


fillWSfactors <- function(session, input, output, i) {
    wsShown.local <- 0
    currentwsfact = input[[paste("wsfact",i,sep="")]]
    currentwsleve = input[[paste("wsleve",i,sep="")]]
    wsleve1toi = 1:i
    wsfact1toi = 1:i
    for (j in 1:i) {
        wsfact1toi[j]=to.identifier(input[[paste("wsfact",j,sep="")]])
        wsleve1toi[j]=to.numeric(input[[paste("wsleve",j,sep="")]])
    }
    if(all(is.numeric(wsleve1toi))) {
        productwsleve = prod(wsleve1toi)
    }

    if ((nchar(currentwsfact) > 0)&(nchar(currentwsleve)>0)) {
        if ((currentwsfact!=to.identifier(currentwsfact))||(currentwsleve!=to.integer(currentwsleve))) {
            updateButton(session, "S2Apply", disabled = TRUE)
        } else {
            if (productwsleve < length(input$superbVariables)) {
                updateButton(session, "S2Apply", disabled = TRUE)
                output[[paste("superbWSFactors",i+1,sep="")]]  <- renderUI({ tagList( wslinefct(i+1) )})
            } else if (productwsleve > length(input$superbVariables)) {
                updateButton(session, "S2Apply", disabled = TRUE) 
                wsShown.local <- i
                for (j in seq(i+1, 4, length.out = 4-i))  { #(i+1):4) 
                    output[[paste("superbWSFactors",j,sep="")]]  <- renderUI({ })
                 }
               # populate and open modal
                makeNonfactorialContent(output, input$superbVariables, wsfact1toi, wsleve1toi )
                toggleModal(session, "superbnonfact", toggle = "open")
                # the levels are collected and validated upon button click
            } else { # all good; a full-factorial design
                wsShown.local <- -i
                for (j in seq(i+1, 4, length.out = 4-i)) {
                    output[[paste("superbWSFactors",j,sep="")]]  <- renderUI({ })
                 }
                updateButton(session, "S2Apply", disabled = FALSE)
            }
        }
    } else updateButton(session, "S2Apply", disabled = TRUE)
    WSFactorsNames  <<- wsfact1toi
    WSFactorsLevels <<- wsleve1toi
    return(wsShown.local)
}

####################################################################
  

####################################################################

theServerFct <- function(input, output, session) {

    # a dummy function for quick and dirty debugging information
    mycat <- function(...) {
        if (!is.null(getOption("superb.shiny"))) {
            if (getOption("superb.shiny") == "display") {
                cat(...) } } }
    mycat("Display of feedback information turned on...\n")
    mycat(appversion, "\n")

    # Information collected as we go through the steps:
    info           <- list() # list with $Step1, $Step2, $Step4, $Step5 and $Step6
    info$Completed <- 0      # is Step 1 completed?

    # Step2 information contains the arguments to superbPlot except...
    info$Step2$WSDesign      <- "fullfactorial" # changed on Step 2
    info$Step2$statistic     <- "mean"          # changed on Step 3 
    info$Step2$errorbar      <- "CI"            # changed on Step 3 
    info$Step2$gamma         <- 0.95            # changed on Step 3 
    info$Step2$plotStyle     <- NULL            # changed on Step 5
    info$Step2$factorOrder   <- NULL            # changed on Step 5
    # ... the adjustments that are in Step4 (they will be sub-listed)...
    info$Step4$purpose       <- "single"        # changed on Step 4
    info$Step4$decorrelation <- "none"          # changed on Step 4
    info$Step4$popSize       <- Inf             # changed on Step 4
    # ... and Step5 and 6 which contains ggplot attributes and directives.
    info$Step5               <- list()          # no graphic attributes added
    info$Step6               <- list()          # no graphic directives added

    wsShown <- 0    # the number of within-subject factor lines shown
    gaShown <- 1    # the number of graphic attribute input box shown
    oldDeleteClicks <- rep(0,5)

    ##########################################################
    ## STEP 1: Load the file and check its validity
    ##########################################################
    # when using the fileInput
    observeEvent(input$superbFile, ({
        mycat("S1: Browse clicked!\n")
        ext <- tools::file_ext(input$superbFile$datapath)
        if (ext %in% c("csv","tsv","sav")) {
            # read the file and return its content in the server environment
            dataToPlot <<- if (ext=="csv") 
                read.csv(input$superbFile$datapath, header = TRUE)
            else if (ext =="tsv")
                read.delim(input$superbFile$datapath, header = TRUE)    
            else if (ext =="sav")
                read.spss(input$superbFile$datapath, to.data.frame = TRUE)      

            # erase the former experimental-design script, if any...
            info$Completed <<- 0

            # erase the message, plot, table and script tabs
            output$superbMessages1   <- renderUI({ NULL })
            output$superbMessages2   <- renderUI({ NULL })
            output$superbPlotCaption <- renderUI({ NULL })
            output$superbPlot        <- renderPlot({  })
            output$superbDataCaption <- renderUI({ NULL })
            output$superbData        <- renderTable({ NULL })
            output$superbScript      <- renderText({ NULL })

            # reset the variables in the selectInput
            updateSelectInput(session, "superbBSFactors", choices  = names(dataToPlot) )
            updateSelectInput(session, "superbVariables", choices  = names(dataToPlot) )
     
            # set the button states
            updateButton(session, "S1Apply", disabled = FALSE)
            updateButton(session, "S1Next",  disabled = TRUE)
        } else {
            updateButton(session, "S1Apply", disabled = TRUE)
            # error unknown file type
            output$superbMessages1   <- renderUI({
                tagList( h4("Step 1: Unknown file type"),
                    p("The file with extension ", ext, " is an unknown file format.")
                ) })           
        }  
    }))


    # When Apply is pressed
    # APPLY validates the choices, put MESSAGES, update SCRIPT, enables NEXT
    observeEvent(input$S1Apply, ({
        mycat("S1: Apply clicked!", "\n")

        # put message
        output$superbMessages1 <- renderUI(tagList(
            h4("Step 1: Loading the data"),
            p("Done. A sneak preview of the file is available under the tab ",em("Summary data"),".")
        ))

        # collect information grabed on Step 1
        info$Step1$ext  <<- tools::file_ext(input$superbFile$datapath)
        info$Step1$name <<- input$superbFile$name
        info$Completed  <<- 1

        # update the script
        script <- generateScript( info )
        output$superbScript <- renderText(paste3(script, collapse="\n\n"))

        # put a sneak preview of the data
        output$superbDataCaption <- renderUI({h4("Table: The first ten lines of the data contained in ",em(input$superbFile$name))})
        output$superbData        <- renderTable({ dataToPlot[1:10,] })

        # lets enable the NEXT button
        updateButton(session, "S1Next", disabled = FALSE)
        })
    )


    # When Next is pressed
    # Nothing to do...



    ##########################################################
    ## STEP 2: Set the design
    ##########################################################

    observeEvent(input$nonfactclose, ({
        mycat("S2: Nonfactorial modal left!", "\n")

        # determine how many WS factors have been given
        wsfactornumbers <<- wsShown
        # extract the factor names
        wsfactornames  <- 1:wsfactornumbers
        for (j in 1:wsfactornumbers) {
            wsfactornames[j]  <- to.identifier(input[[paste("wsfact",j,sep="")]])
        }
        # collect the non-factorial design levels        
        res = list()
        for (i in input$superbVariables) {
            sub=c()
            for (j in wsfactornames) {
                sub = c(sub, input[[paste("wscell",i,j,sep="")]])   
            }
            res[[length(res)+1]] = sub
        }
        # return the non-factorial design levels
        info$Step2$WSDesign <<- res
        updateButton(session, "S2Apply", disabled = FALSE)
    }))


    # for the two inputselect dropdown lists, remove the variables used
    observeEvent(input$superbBSFactors, ({
        mycat("S2: selecting BSFactors", "\n")
        updateButton(session, "S2Next", disabled = TRUE)
        l1 <- input$superbBSFactors    
        l2 <- input$superbVariables    
        r2 <- setdiff(names(dataToPlot), l1)
        updateSelectInput(session, "superbVariables", choices = r2, selected = l2)
        updateSelectInput(session, "superbBSFactors", selected = l1)
    }), ignoreInit = TRUE) # kept separate for ergonomics

    observeEvent(input$superbVariables, ({
        mycat("S2: selecting Variables or BSFactors", "\n")
        updateButton(session, "S2Next", disabled = TRUE)
        l1 <- input$superbBSFactors    
        l2 <- input$superbVariables    
        r1 <- setdiff(names(dataToPlot), l2)
        r2 <- setdiff(names(dataToPlot), l1)
        updateSelectInput(session, "superbBSFactors", choices = r1, selected = l1)
        updateSelectInput(session, "superbVariables", selected = l2)

        # when more than one variables, requires WSFactor names.
        if (length(input$superbVariables)==1) {
            updateButton(session, "S2Apply", disabled = FALSE)
            output$superbWSFactors1  <- renderUI({ })
            output$superbWSFactors2  <- renderUI({ })
            output$superbWSFactors3  <- renderUI({ })
            output$superbWSFactors4  <- renderUI({ })
            WSFactorsNames  <<- NULL
            WSFactorsLevels <<- NULL
        } else if (length(input$superbVariables) > 1) {
            updateButton(session, "S2Apply", disabled = TRUE)
            output$superbWSFactors1  <- renderUI({ tagList(
                p(strong("Define within-subject factors")),
                wslinefct(1)
            )})
        } else { #if variables empty
            updateButton(session, "S2Apply", disabled = TRUE)
        }
    }), ignoreNULL = FALSE, ignoreInit = TRUE) 

    # when within-subject design, reads the wsfactors with their levels
    observeEvent({list(input$wsfact1, input$wsleve1)}, ({
        updateButton(session, "S2Next", disabled = TRUE)
        wsShown <<- fillWSfactors(session, input, output, 1)
        if (wsShown < 0) { info$Step2$WSDesign = "fullfactorial" }
    }), ignoreNULL=FALSE, ignoreInit = TRUE)

    observeEvent({list(input$wsfact2, input$wsleve2)}, ({
        updateButton(session, "S2Next", disabled = TRUE)
        wsShown <<- fillWSfactors(session, input, output, 2)
        if (wsShown < 0) { info$Step2$WSDesign = "fullfactorial" }
    }), ignoreNULL=FALSE, ignoreInit = TRUE)

    observeEvent({list(input$wsfact3, input$wsleve3)}, ({
        updateButton(session, "S2Next", disabled = TRUE)
        wsShown <<- fillWSfactors(session, input, output, 3)
        if (wsShown < 0) { info$Step2$WSDesign = "fullfactorial" }
    }), ignoreNULL=FALSE, ignoreInit = TRUE)

    observeEvent({list(input$wsfact4, input$wsleve4)}, ({
        updateButton(session, "S2Next", disabled = TRUE)
        wsShown <<- fillWSfactors(session, input, output, 4)
        if (wsShown < 0) { info$Step2$WSDesign = "fullfactorial" }
    }), ignoreNULL=FALSE, ignoreInit = TRUE)


    # When Apply is pressed
    # APPLY validates the choices, put MESSAGES, update SCRIPT, enables NEXT
    observeEvent(input$S2Apply, ({
        mycat("S2: Apply clicked!", "\n")
        # run some checks!!!
        if (!identical(unique(c(input$superbBSFactors, WSFactorsNames)),c(input$superbBSFactors, WSFactorsNames))) {
            output$superbMessages2 <- renderUI(
                tagList( h4("Step 2: Inconsistent input"),
                    p("Some of the factor names are repeated. Use only unique names.")
                ))
            updateTabsetPanel(session, "MainDisplay", selected = "Messages")
        } else if (length(WSFactorsNames)+length(input$superbBSFactors)>4) {
            output$superbMessages2 <- renderUI(
                tagList( h4("Step 2: Inconsistent input"),
                    p("More than four factors are specifed. superb can only handle up to four factors in total.")
                ))
            updateTabsetPanel(session, "MainDisplay", selected = "Messages")
        } else {
            # collect information obtained on Step 2
            info$Step2$data        <<- dataToPlot
            info$Step2$BSFactors   <<- input$superbBSFactors
            info$Step2$WSFactors   <<- if(length(WSFactorsNames)>0) {
                paste(paste(WSFactorsNames,WSFactorsLevels, sep="("),")",sep="")
            } else {NULL}
            info$Step2$factorOrder <<- NULL        # reset as set on Step 5
            info$Step2$variables   <<- input$superbVariables
            info$Completed         <<- 2
    
            # update script
            script <- generateScript( info )
            output$superbScript <- renderText(paste3(script, collapse="\n\n"))

            # get the messages, errors and warnings; and the plot, and the data
            runAndShowIt( input, output, info )

            # lets enable the NEXT button
            updateButton(session, "S2Next", disabled = FALSE)
            }
        })
    ) # end APPLY


    # When Next is pressed
    # nothing to do here



    ##########################################################
    ## STEP 3: 
    ##########################################################


    observeEvent({list(input$superbStatistic, input$superbErrorbar)}, ({
        mycat("S3: choices of functions", input$superbStatistic, "and", input$superbErrorbar, "\n")

        f     <- TRUE
        ebfct <- paste(input$superbErrorbar, input$superbStatistic, sep=".")

        # check that it is a valid statistics
        if (!superb:::is.stat.function(input$superbStatistic))  {f <- FALSE}
        if (!superb:::is.errorbar.function(ebfct))              {f <- FALSE}
        if (superb:::is.gamma.required(ebfct)) {
            output$superbGamma <- renderUI({
                    textInput("gamma", "Coverage", value = "0.95", width = "100px", placeholder = "coverage level")
                })
        } else {
            output$superbGamma <- renderUI({ NULL })
        }
        if (f) {
            updateButton(session, "S3Apply", disabled = FALSE)
        } else {
            updateButton(session, "S3Apply", disabled = TRUE)
            updateButton(session, "S3Next", disabled = TRUE)
        }
        }), ignoreNULL=FALSE, ignoreInit = FALSE
    )


    # When Apply is pressed
    # APPLY validates the choices, put MESSAGES, update SCRIPT, enables NEXT
    observeEvent(input$S3Apply, ({
        mycat("S3: Apply clicked!", "\n")

        # update script
        script <- generateScript( info )
        output$superbScript <- renderText(paste3(script, collapse="\n\n"))

        # collect information grabed on Step 3
        ebfct <- paste(input$superbErrorbar, input$superbStatistic, sep=".")
        info$Step2$statistic <<- input$superbStatistic
        info$Step2$errorbar  <<- input$superbErrorbar
        info$Step2$gamma     <<- NULL
        if (superb:::is.gamma.required(ebfct)) {
            if (is.something(input$gamma)) {
                info$Step2$gamma  <<- to.numeric(input$gamma)
            }
        } 

        # get the messages, errors and warnings; and the plot, and the data
        runAndShowIt( input, output, info )

        # lets enable the NEXT button
        updateButton(session, "S3Next", disabled = FALSE)

        })
    )


    # When Next is pressed
    # nothing to do here



    ##########################################################
    ## STEP 4: 
    ##########################################################

    # disable decorrelation if no within-subject factors
    observeEvent( {list(input$superbPurpose,input$S3Next)}, ({
        mycat("S4: purpose is",input$superbPurpose,"\n")
        output$S4Decorrelation <- if (
            (is.null(info$Step2$WSFactors))||(input$superbPurpose == "single")
        ) {
            renderUI( tagList({NULL}) )
        } else {
            renderUI( radioButtons("superbDecorrelation", "Select the decorrelation method",
                choiceNames  = c("None","Cousineau-Morey (2005, 2008)", "Correlation adjusted","Loftus and Masson (1994)","Local decorrelation (radius 3)"),
                choiceValues = c("none","CM","CA","LM","LD3"), selected="none"
            ))
        }
        })
    )


    # When Apply is pressed
    # APPLY validates the choices, put MESSAGES, update SCRIPT, enables NEXT
    observeEvent(input$S4Apply, ({
        mycat("S4: Apply clicked!", "\n")
        # some checks
        if ((input$superbPopsize!="Inf")&((to.numeric(input$superbPopsize)<1)||(as.character(format(to.numeric(input$superbPopsize),scientific=FALSE))!=str_trim(input$superbPopsize)))) {
            output$superbMessages2 <- renderUI(
                tagList( h4("Step 2: Inconsistent input"),
                    p("Population size must be `Inf` (infinite) or a positive integer.")
                ))
            updateTabsetPanel(session, "MainDisplay", selected = "Messages")
        } else {
            # collect information grabed on Step 4               
            info$Step4$purpose       <<- if (is.something(input$superbPurpose)) input$superbPurpose else "single"
            info$Step4$decorrelation <<- if (is.something(input$superbDecorrelation)) input$superbDecorrelation else "none"
            info$Step4$popSize       <<- to.numeric(input$superbPopsize)

            # update script
            script <- generateScript( info )
            output$superbScript <- renderText(paste3(script, collapse="\n\n"))

            # get the messages, errors and warnings; and the plot, and the data
            runAndShowIt( input, output, info )

            # lets enable the NEXT button
            updateButton(session, "S4Next", disabled = FALSE)
        }
        })
    ) # end APPLY


    # When Next is pressed
    # set the factors in the superbPlotorder line 299
    observeEvent( input$S4Next , ({
        updateSelectInput(session, "superbPlotorder", 
            choices = c(WSFactorsNames, input$superbBSFactors), selected = c( WSFactorsNames, input$superbBSFactors) )
    }))

    

    ##########################################################
    ## STEP 5: 
    ##########################################################
    # adding additional directives when needed
    observeEvent( {list(input$content1, input$content2, input$content3, input$content4, input$content5)}, ({
        mycat("S5: graphic attributes manipulated!", "\n")
        contents <- list(input$content1, input$content2, input$content3, input$content4, input$content5 )
        gaN <- length(contents[sapply(contents, is.something)])
        if (gaN <= gaShown ) {
            output[[paste("linedir",gaN+1,sep="")]]  <- renderUI({ tagList( galinefct(gaN+1) )})
            gaShown <<- gaN+1
        }
    }), ignoreInit = TRUE ) 

    # removing additional directives when needed
    observeEvent( {list(input$delete1, input$delete2, input$delete3, input$delete4, input$delete5)}, ({
        mycat("S5: graphic attributes to delete!", "\n")
        newclicks <- list(input$delete1, input$delete2, input$delete3, input$delete4, input$delete5)

        if (gaShown > 1) {
            newDeleteClicks <- rep(0,5)
            for (i in 1:5) 
                newDeleteClicks[i] <- if (is.null(newclicks[[i]])) 0 else newclicks[[i]][1]
            buttonClicked <- match(1,newDeleteClicks - oldDeleteClicks)

            if (!is.na(buttonClicked)) {
                for (i in buttonClicked:gaShown) {
                    # move up one notch all the graphic attributes
                    updateSelectInput(session, paste("directive",i,sep=""), 
                        selected = input[[paste("directive",i+1,sep="")]] )
                    updateTextInput(session, paste("content",i,sep=""), 
                        value = input[[paste("content",i+1,sep="")]] )
                }
                # hide the line
                output[[paste("linedir",gaShown,sep="")]] <- renderUI({NULL})

                # keep track of the button clicked
                gaShown <<- gaShown-1
                oldDeleteClicks <<- newDeleteClicks
            }
        }
    }), ignoreNULL=TRUE, ignoreInit = TRUE ) 

    # When Apply is pressed
    # APPLY validates the choices, put MESSAGES, update SCRIPT, enables NEXT
    observeEvent(input$S5Apply, ({
        mycat("S5: Apply clicked!", "\n")
        # some checks
        if (length(input$superbPlotorder)<length(c(WSFactorsNames, input$superbBSFactors))) {
            output$superbMessages2 <- renderUI(
                tagList( h4("Step 2: Inconsistent input"),
                    p("You must place all ",length(c(WSFactorsNames, input$superbBSFactors))," factors in 'Select the order...")
                ))
            updateTabsetPanel(session, "MainDisplay", selected = "Messages")
        } else {
            # collect information grabed on Step 5
            info$Step2$factorOrder   <<- input$superbPlotorder
            info$Step2$plotStyle     <<- input$superbLayout

            # reset the whole attributes
            info$Step5 <<- list()
            info$Step5[[input$directive1]] <<- if ((is.something(input$content1))&(is.something(input$directive1))) 
                input$content1 else NULL
            if ((is.something(input$content2))&(is.something(input$directive2))) 
                info$Step5[[input$directive2]] <<- input$content2
            if ((is.something(input$content3))&(is.something(input$directive3))) 
                info$Step5[[input$directive3]] <<- input$content3
            if ((is.something(input$content4))&(is.something(input$directive4))) 
                info$Step5[[input$directive4]] <<- input$content4
            if ((is.something(input$content5))&(is.something(input$directive5))) 
                info$Step5[[input$directive5]] <<- input$content5

            if (is.something(input$ornates)) 
                info$Step6 <<- input$ornates
            else info$Step6 <<- list()
            info$Completed           <<- 5

            # update script
            script <- generateScript( info )
            output$superbScript <- renderText(paste3(script, collapse="\n\n"))

            # get the messages, errors and warnings; and the plot, and the data
            runAndShowIt( input, output, info )

            # lets enable the NEXT button
            updateButton(session, "S5Next", disabled = FALSE)
        }
        })
    ) # end APPLY


    ##########################################################
    ## NAVIGATE BETWEEN THE PANES 
    ##########################################################
    observeEvent(input$S1Next, ({ updateCollapse(session, "collapseExample", open = 2) }))
    observeEvent(input$S2Prev, ({ updateCollapse(session, "collapseExample", open = 1) }))
    observeEvent(input$S2Next, ({ updateCollapse(session, "collapseExample", open = 3) }))
    observeEvent(input$S3Prev, ({ updateCollapse(session, "collapseExample", open = 2) }))
    observeEvent(input$S3Next, ({ updateCollapse(session, "collapseExample", open = 4) }))
    observeEvent(input$S4Prev, ({ updateCollapse(session, "collapseExample", open = 3) }))
    observeEvent(input$S4Next, ({ updateCollapse(session, "collapseExample", open = 5) }))
    observeEvent(input$S5Prev, ({ updateCollapse(session, "collapseExample", open = 4) }))
    observeEvent(input$S5Next, ({ updateCollapse(session, "collapseExample", open = 6) }))
    observeEvent(input$S6Prev, ({ updateCollapse(session, "collapseExample", open = 5) }))


    # Adjuste the plot size
    observeEvent( {list(input$height,input$width)}, ({
        if (info$Completed >=2 )
            runAndShowIt( input, output, info )
    }))

  }



##########################################################
## THIS IS IS! RUN THE PROCESS
##########################################################


shinyApp(ui = thePage, server = theServerFct)


