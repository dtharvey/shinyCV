library(shiny)

navbarPage("CV Simulator",

  tabPanel("About",
           fixedPage(
             fixedRow(
               column(8,
    includeHTML("about.html")
           ),
    column(4,
           img(src="about_cv.png", align = "center"),
           img(src="about_diff.png", align = "center")
           )
           )
    )
           ),
  tabPanel("Parameters",
    includeHTML("parameters.html")
           ),
  tabPanel("Mechanism: E",
    fixedPage(
      fixedRow(
        column(4, align = "center",
          plotOutput("PvsT_e", width = "100%", height = "200px"
                    ),
          sliderInput(inputId = "cursor_e",
                      label = NULL,
                      min = 0, max = 1, value = 0, step = 0.01,
                      sep = "", round = -2, ticks = FALSE),
          helpText(HTML("<center><b>time indicated by black dot (s)</b></center>"))
              ),
        column(4, align = "center",
          plotOutput("IvsP_e", width = "100%", height = "300px"
                    )
              ),
        column(4, align = "center",
          plotOutput("CvsD_e", width = "100%", height = "200px"
                      ),
          sliderInput(inputId = "scale_e", label = NULL,
                      min = 0, max = 1, value = c(0,1), step = 0.01
                      ),
          helpText(HTML("<center><b>concentration limits</b></center>"))
              )
      ), # end fixed row
      wellPanel(
      fixedRow(
        column(2, offset = 1, align = "center",
          h4("Redox Reaction"),
          radioButtons(inputId = "direction_e", label = "direction",
                      choices = c("Ox --> Red" = 1,
                                  "Red --> Ox" = 2),
                      selected = 1
                      ),
          radioButtons(inputId = "n_e",
                       label = "number of electrons",
                       choices = c(1, 2, 3), selected = 1,
                       inline = TRUE
                       ),
          sliderInput(inputId = "alpha_e",
                      label = HTML("transfer coefficient"),
                      min = 0.1, max = 0.9, value = 0.5,
                      step = 0.01,
                      ticks = FALSE
                      )
               ),
        column(2, align = "center",
          h4("Redox Species"),
          sliderInput(inputId = "formal_e",
                      label = HTML("<em>E</em><sup>o</sup>&prime; (V)"),
                      min = -1.5, max = 1.5, step = 0.01, value = 0,
                      ticks = FALSE
                      ),
          sliderInput(inputId = "concbulk_e", label = "bulk [Ox] (mM)",
                      min = 0.1, max = 10, step = 0.001, value = 1,
                      ticks = FALSE
                      ),
          sliderInput(inputId = "difcoef_e",
                      label = HTML("<em>D</em> (cm<sup>2</sup>/s)"),
                      min = 0.1, max = 10,
                      step = 0.01, value = 1, ticks = FALSE,
                      post = HTML("x10<sup>-5</sup>")
                       )
               ),
        column(2, align = "center",
          h4("Potentials"),
          sliderInput(inputId = "start_e",
                      label = HTML("<em>E</em><sub>start</sub> (V)"),
                      ticks = FALSE, value = 0.5,
                      min = -2, max = 2, step = 0.001
                      ),
          sliderInput(inputId = "switch_e",
                      label = HTML("<em>E</em><sub>switch</sub> (V)"),
                      ticks = FALSE, value = -0.5,
                      min = -2, max = 2, step = 0.001
                      ),
          sliderInput(inputId = "scanrate_e",
                      label = "scan rate (V/s)",
                      value = 0, min = -3, max = 3, step = 0.1,
                      ticks = FALSE, pre = "10^"
                      )
               ),
        column(2, align = "center",
          h4("Rate Constants"),
          sliderInput(inputId = "ko_e",
                      label = HTML("<em>k</em><sup>o</sup> (cm/s)"),
                      min = -3, max = 3, step = 0.1, value = 0,
                      ticks = FALSE, pre = "10^"
                       )
               ),
        column(2, align = "center",
          h4("Miscellaneous"),
          sliderInput(inputId = "area_e",
                      label = HTML("electrode area (cm<sup>2</sup>)"),
                      ticks = FALSE, value = 0.01,
                      min = 0.001, max = 0.1, step = 0.001
                      ),
          sliderInput(inputId = "temp_e",
                      label = "temperature (K)", ticks = FALSE,
                      value = 298, min = 280, max = 370, step = 1
                      ),
          actionButton(inputId = "reset_e", label = "reset")
               )
      ) # end fixed row
      ) # end well panel
    ) # end fixed page

  ), # end tab panel

  tabPanel("Mechanism: EC",
    fixedPage(
      fixedRow(
        column(4, align = "center",
               plotOutput("PvsT_ec", width = "100%",
                          height = "200px"
               ),
               sliderInput(inputId = "cursor_ec",
                           label = NULL,
                           min = 0, max = 1, value = 0, step = 0.01,
                           sep = "", round = -2, ticks = FALSE),
               helpText(HTML("<center><b>time indicated by black dot (s)</b></center>"))
        ),
        column(4, align = "center",
               plotOutput("IvsP_ec", width = "100%",
                          height = "300px"
               )
        ),
        column(4, align = "center",
               plotOutput("CvsD_ec", width = "100%",
                          height = "200px"
               ),
               sliderInput(inputId = "scale_ec", label = NULL,
                           min = 0, max = 1, value = c(0,1),
                           step = 0.01
               ),
               helpText(HTML("<center><b>concentration limits</b></center>"))
        )
      ), # end fixed row
      wellPanel(
      fixedRow(
        column(2, offset = 1, align = "center",
               h4("Redox Reaction"),
               radioButtons(inputId = "direction_ec", label = "direction",
                           choices = c("Ox --> Red" = 1, "Red --> Ox" = 2),
                           selected = 1
               ),
               radioButtons(inputId = "n_ec", label = "number of electrons",
                            choices = c(1, 2, 3), selected = 1,
                            inline = TRUE
               ),
               sliderInput(inputId = "alpha_ec",
                           label = HTML("transfer coefficient"),
                           min = 0.1, max = 0.9, value = 0.5, step = 0.01,
                           ticks = FALSE
               )
        ),
        column(2, align = "center",
               h4("Redox Species"),
               sliderInput(inputId = "formal_ec",
                           label = HTML("<em>E</em><sup>o</sup>&prime; (V)"),
                           min = -1.5, max = 1.5, step = 0.01, value = 0,
                           ticks = FALSE
               ),
               sliderInput(inputId = "concbulk_ec", label = "bulk [Ox] (mM)",
                           min = 0.1, max = 10, step = 0.001, value = 1,
                           ticks = FALSE
               ),
               sliderInput(inputId = "difcoef_ec",
                           label = HTML("<em>D</em> (cm<sup>2</sup>/s)"),
                           min = 0.1, max = 10,
                           step = 0.01, value = 1, ticks = FALSE,
                           post = HTML("x10<sup>-5</sup>")
               )
        ),
        column(2, align = "center",
               h4("Potentials"),
               sliderInput(inputId = "start_ec",
                           label = HTML("<em>E</em><sub>start</sub> (V)"),
                           ticks = FALSE, value = 0.5,
                           min = -2, max = 2, step = 0.001
               ),
               sliderInput(inputId = "switch_ec",
                           label = HTML("<em>E</em><sub>switch</sub> (V)"),
                           ticks = FALSE, value = -0.5,
                           min = -2, max = 2, step = 0.001
               ),
               sliderInput(inputId = "scanrate_ec",
                           label = "scan rate (V/s)",
                           value = 0, min = 0, max = 3, step = 0.1,
                           ticks = FALSE, pre = "10^"
               )
        ),
        column(2, align = "center",
               h4("Rate Constants"),
               sliderInput(inputId = "ko_ec",
                           label = HTML("<em>k</em><sup>o</sup> (cm/s)"),
                           min = -3, max = 3, step = 0.1, value = 0,
                           ticks = FALSE, pre = "10^"
               ),
               sliderInput(inputId = "kcf_ec",
                          label = HTML("<em>k</em><sub>chem,f</sub> (s<sup>-1</sup>)"),
                          min = -2, max = 2, step = 0.1, value = 0,
                          ticks = FALSE, pre = "10^"
               ),
               sliderInput(inputId = "kcr_ec",
                          label = HTML("<em>k</em><sub>chem,r</sub> (s<sup>-1</sup>)"),
                           min = -2, max = 2, step = 0.1, value = 0,
                           ticks = FALSE, pre = "10^"
               )
        ),
        column(2, align = "center",
               h4("Miscellaneous"),
               sliderInput(inputId = "area_ec",
                           label = HTML("electrode area (cm<sup>2</sup>)"),
                           ticks = FALSE, value = 0.01,
                           min = 0.001, max = 0.1, step = 0.001
               ),
               sliderInput(inputId = "temp_ec",
                           label = "temperature (K)", ticks = FALSE,
                           value = 298, min = 280, max = 370, step = 1
               ),
               actionButton(inputId = "reset_ec", label = "reset")
        )
      ) # end fixed row
      ) # end well panel
    ) # end fixed page
  ), # end tab panel

    tabPanel("Mechanism: CE",
      fixedPage(
        fixedRow(
          column(4, align = "center",
                 plotOutput("PvsT_ce", width = "100%", height = "200px"
                 ),
                 sliderInput(inputId = "cursor_ce",
                             label = NULL,
                             min = 0, max = 1, value = 0, step = 0.01,
                             sep = "", round = -2, ticks = FALSE),
                 helpText(HTML("<center><b>time indicated by black dot (s)</b></center>"))
          ),
          column(4, align = "center",
                 plotOutput("IvsP_ce", width = "100%", height = "300px"
                 )
          ),
          column(4, align = "center",
                 plotOutput("CvsD_ce", width = "100%", height = "200px"
                 ),
                 sliderInput(inputId = "scale_ce", label = NULL,
                             min = 0, max = 1, value = c(0,1), step = 0.01
                 ),
                 helpText(HTML("<center><b>concentration limits</b></center>"))
          )
        ), # end fixed row
        wellPanel(
        fixedRow(
          column(2, offset = 1, align = "center",
                 h4("Redox Reaction"),
                 radioButtons(inputId = "direction_ce",
                              label = "direction",
                             choices = c("Ox --> Red" = 1,
                                         "Red --> Ox" = 2),
                             selected = 1
                 ),
                 radioButtons(inputId = "n_ce", label = "number of electrons",
                              choices = c(1, 2, 3), selected = 1,
                              inline = TRUE
                 ),
                 sliderInput(inputId = "alpha_ce",
                             label = HTML("transfer coefficient"),
                             min = 0.1, max = 0.9, value = 0.5, step = 0.01,
                             ticks = FALSE
                 )
          ),
          column(2, align = "center",
                 h4("Redox Species"),
                 sliderInput(inputId = "formal_ce",
                             label = HTML("<em>E</em><sup>o</sup>&prime; (V)"),
                             min = -1.5, max = 1.5, step = 0.01, value = 0,
                             ticks = FALSE
                 ),
                 sliderInput(inputId = "concbulk_ce", label = "bulk [Z + Ox] (mM)",
                             min = 0.1, max = 10, step = 0.001, value = 1,
                             ticks = FALSE
                 ),
                 sliderInput(inputId = "difcoef_ce",
                             label = HTML("<em>D</em> (cm<sup>2</sup>/s)"),
                             min = 0.1, max = 10,
                             step = 0.01, value = 1, ticks = FALSE,
                             post = HTML("x10<sup>-5</sup>")
                 )
          ),
          column(2, align = "center",
                 h4("Potentials"),
                 sliderInput(inputId = "start_ce",
                             label = HTML("<em>E</em><sub>start</sub> (V)"),
                             ticks = FALSE, value = 0.5,
                             min = -2, max = 2, step = 0.001
                 ),
                 sliderInput(inputId = "switch_ce",
                             label = HTML("<em>E</em><sub>switch</sub> (V)"),
                             ticks = FALSE, value = -0.5,
                             min = -2, max = 2, step = 0.001
                 ),
                 sliderInput(inputId = "scanrate_ce",
                             label = "scan rate (V/s)",
                             value = 0, min = 0, max = 3, step = 0.1,
                             ticks = FALSE, pre = "10^"
                 )
          ),
          column(2, align = "center",
                 h4("Rate Constants"),
                 sliderInput(inputId = "ko_ce",
                             label = HTML("<em>k</em><sup>o</sup> (cm/s)"),
                             min = -3, max = 3, step = 0.1, value = 0,
                             ticks = FALSE, pre = "10^"
                 ),
                 sliderInput(inputId = "kcf_ce",
                          label = HTML("<em>k</em><sub>chem,f</sub> (s<sup>-1</sup>)"),
                             min = -2, max = 2, step = 0.1, value = 0,
                             ticks = FALSE, pre = "10^"
                 ),
                 sliderInput(inputId = "kcr_ce",
                          label = HTML("<em>k</em><sub>chem,r</sub> (s<sup>-1</sup>)"),
                             min = -2, max = 2, step = 0.1, value = 0,
                             ticks = FALSE, pre = "10^"
                 )
          ),
          column(2, align = "center",
                 h4("Miscellaneous"),
                 sliderInput(inputId = "area_ce",
                             label = HTML("electrode area (cm<sup>2</sup>)"),
                             ticks = FALSE, value = 0.01,
                             min = 0.001, max = 0.1, step = 0.001
                 ),
                 sliderInput(inputId = "temp_ce",
                             label = "temperature (K)", ticks = FALSE,
                             value = 298, min = 280, max = 370, step = 1
                 ),
                 actionButton(inputId = "reset_ce", label = "reset")
          )
        ) # end fixed row
        ) # end well panel
      ) # end fixed page
    ), # end tab panel

  tabPanel("Exercises",
   fixedPage(
     includeHTML("explore.html")
   ) # end fixed page
  ), # end tab panel

  tabPanel("Computational Details",
    fixedPage(
      includeHTML("comp_details.html")
    ) # end fixed page
  ), # end tab panel

  tabPanel("Resources",
    fixedPage(
      includeHTML("references.html")
    ) # end fixed page
  )

) # end nav bar page

