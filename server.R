library(shiny)
source(file = "cvSim.R")

shinyServer(function(input, output, session) {

#' code for mechanism E tab

#' observe the formal potential and the reaction direction and update the sliders for selecting the minimum, maximum, and inital values for the starting potential and the switching potential, and the species that define the bulk concentration

  observe({
    val_e = input$formal_e
    dir_e = input$direction_e
    if (dir_e == 1) {
      updateSliderInput(session, "start_e", min = val_e + 0.25)
      updateSliderInput(session, "start_e", max = +2)
      updateSliderInput(session, "switch_e", max = val_e - 0.25)
      updateSliderInput(session, "switch_e", min = -2)
      updateSliderInput(session, "start_e", value = val_e + 0.5)
      updateSliderInput(session, "switch_e", value = val_e - 0.5)
      updateNumericInput(session, "concbulk_e",
                         label = "bulk [Ox] (mM)")
    } else {
      updateSliderInput(session, "start_e", min = -2)
      updateSliderInput(session, "start_e", max = val_e - 0.25)
      updateSliderInput(session, "switch_e", min = val_e +  0.25)
      updateSliderInput(session, "switch_e", max = +2)
      updateSliderInput(session, "start_e", value = val_e - 0.5)
      updateSliderInput(session, "switch_e", value = val_e + 0.5)
      updateNumericInput(session, "concbulk_e",
                         label = "bulk [Red] (mM)")
    }
  })

#' check to see if the reset button is clicked and, if yes, then reset the original parameters for that mechanism

  observe({
    action_e = input$reset_e
    if(action_e !=0){
      updateRadioButtons(session, "direction_e",
                        selected = 1)
      updateRadioButtons(session, "n_e", selected = 1)
      updateSliderInput(session, "alpha_e", value = 0.5)
      updateSliderInput(session, "formal_e", value = 0)
      updateSliderInput(session, "concbulk_e",
                        label = "bulk [Ox] (mM)", value = 1)
      updateSliderInput(session, "difcoef_e", value = 1)
      updateSliderInput(session, "start_e", min = -2)
      updateSliderInput(session, "start_e", max = 2)
      updateSliderInput(session, "start_e", value = 0.5)
      updateSliderInput(session, "switch_e", min = -2)
      updateSliderInput(session, "switch_e", max = 2)
      updateSliderInput(session, "switch_e", value = -0.5)
      updateSliderInput(session, "scanrate_e", value = 0)
      updateSliderInput(session, "ko_e", value = 0)
      updateSliderInput(session, "area_e", value = 0.01)
      updateSliderInput(session, "temp_e", value = 298)
      updateSliderInput(session, "scale_e", value = c(0,1))
      updateSliderInput(session, "cursor_e", value = 0)
    }
  })

  observe({
    action_ce = input$reset_ce
    if(action_ce !=0){
      updateRadioButtons(session, "direction_ce",
                         selected = 1)
      updateRadioButtons(session, "n_ce", selected = 1)
      updateSliderInput(session, "alpha_ce", value = 0.5)
      updateSliderInput(session, "formal_ce", value = 0)
      updateSliderInput(session, "concbulk_ce",
                        label = "bulk [Ox] (mM)", value = 1)
      updateSliderInput(session, "difcoef_ce", value = 1)
      updateSliderInput(session, "start_ce", min = -2)
      updateSliderInput(session, "start_ce", max = 2)
      updateSliderInput(session, "start_ce", value = 0.5)
      updateSliderInput(session, "switch_ce", min = -2)
      updateSliderInput(session, "switch_ce", max = 2)
      updateSliderInput(session, "switch_ce", value = -0.5)
      updateSliderInput(session, "scanrate_ce", value = 0)
      updateSliderInput(session, "ko_ce", value = 0)
      updateSliderInput(session, "kcf_ce", value = 0)
      updateSliderInput(session, "kcr_ce", value = 0)
      updateSliderInput(session, "area_ce", value = 0.01)
      updateSliderInput(session, "temp_ce", value = 298)
      updateSliderInput(session, "scale_ce", value = c(0,1))
      updateSliderInput(session, "cursor_ce", value = 0)
    }
  })

  observe({
    action_ec = input$reset_ec
    if(action_ec !=0){
      updateRadioButtons(session, "direction_ec",
                         selected = 1)
      updateRadioButtons(session, "n_ec", selected = 1)
      updateSliderInput(session, "alpha_ec", value = 0.5)
      updateSliderInput(session, "formal_ec", value = 0)
      updateSliderInput(session, "concbulk_ec",
                        label = "bulk [Ox] (mM)", value = 1)
      updateSliderInput(session, "difcoef_ec", value = 1)
      updateSliderInput(session, "start_ec", min = -2)
      updateSliderInput(session, "start_ec", max = 2)
      updateSliderInput(session, "start_ec", value = 0.5)
      updateSliderInput(session, "switch_ec", min = -2)
      updateSliderInput(session, "switch_ec", max = 2)
      updateSliderInput(session, "switch_ec", value = -0.5)
      updateSliderInput(session, "scanrate_ec", value = 0)
      updateSliderInput(session, "ko_ec", value = 0)
      updateSliderInput(session, "kcf_ec", value = 0)
      updateSliderInput(session, "kcr_ec", value = 0)
      updateSliderInput(session, "area_ec", value = 0.01)
      updateSliderInput(session, "temp_ec", value = 298)
      updateSliderInput(session, "scale_ec", value = c(0,1))
      updateSliderInput(session, "cursor_ec", value = 0)
    }
  })

#' calculate the time to complete the full scan and update the maximum limit and step for the cursor's slider

  observe({
    ct_e = round(abs(2 * (input$start_e - input$switch_e))/10^(input$scanrate_e), digits = 3)
    if(ct_e > 1){
    updateSliderInput(session, "cursor_e", max = ceiling(ct_e), step = ct_e/100)
    } else {
      updateSliderInput(session, "cursor_e", max = ct_e, step = ct_e/100)
    }
  })

#' observe the bulk concentration and use it to set the maximum concentration for the diffusion profile slider

  observe({
    maxy_e = input$concbulk_e
    updateSliderInput(session, "scale_e", max = maxy_e)
    updateSliderInput(session, "scale_e", value = c(0, maxy_e))
  })

#' grab the value from the cursor for time

  # cursor_e = reactive({
  #   x = input$cursor_e
  #   x
  # })

#' pass values to the cvSim function and return to the object cv_e

  cv_e = reactive({
    out = cvSim(mechanism = "E", e.start = input$start_e,
                e.switch = input$switch_e, e.form = input$formal_e,
                ko = 10^(input$ko_e), kcf = 0, kcr =  0,
                n = as.numeric(input$n_e), alpha = input$alpha_e,
                d = input$difcoef_e * 1e-5, area = input$area_e,
                scan.rate = 10^(input$scanrate_e),
                conc.bulk = input$concbulk_e/1000,
                temp = input$temp_e, t.units = 2000, x.units = 180,
                sd.noise = 0)
    out
  })

#' plot the applied potential wavelform and add dot for the cursor

  output$PvsT_e = renderPlot({
    old.par = par(mar = c(4.1, 4.1, 1.0, 2.1), mgp = c(2, 1, 0))
    plotPotential(cv_e(), main_title = "Applied Potential Waveform")
    id = which.min(abs(cv_e()$time - (input$cursor_e)))
    points(x = cv_e()$time[id],
           y = cv_e()$potential[id],
           pch = 19, cex = 1.5, col = "black")
    par(old.par)

  })

  #' plot the annotated CV and add dot for the cursor

  output$IvsP_e = renderPlot({
    old.par = par(mar = c(4.1, 4.1, 1.0, 2.1), mgp = c(2, 1, 0))
    annotateCV(cv_e())
    title(main = "Cyclic Voltammogram")
    id = which.min(abs(cv_e()$time - (input$cursor_e)))
    points(x = cv_e()$potential[id],
           y = cv_e()$current[id],
           pch = 19, cex = 1.5, col = "black")
    par(old.par)
  })

#' plot the diffusion profiles at the time given by the cursor

output$CvsD_e = renderPlot({
  old.par = par(mar = c(4.1, 4.1, 1.00, 2.1), mgp = c(2, 1, 0))
  plotDiffusion(cv_e(), t = input$cursor_e,
                miny = input$scale_e[1], maxy = input$scale_e[2])
  par(old.par)
})

#' code for mechanism EC tab; see above for notes on the code

observe({
  val_ec = input$formal_ec
  dir_ec = input$direction_ec
  if (dir_ec == 1) {
    updateSliderInput(session, "start_ec", min = val_ec + 0.25)
    updateSliderInput(session, "start_ec", max = +2)
    updateSliderInput(session, "switch_ec", max = val_ec - 0.25)
    updateSliderInput(session, "switch_ec", min = -2)
    updateSliderInput(session, "start_ec", value = val_ec + 0.5)
    updateSliderInput(session, "switch_ec", value = val_ec - 0.5)
    updateNumericInput(session, "concbulk_ec",
                       label = "bulk [Ox] (mM)")
  } else {
    updateSliderInput(session, "start_ec", min = -2)
    updateSliderInput(session, "start_ec", max = val_ec - 0.25)
    updateSliderInput(session, "switch_ec", min = val_ec +  0.25)
    updateSliderInput(session, "switch_ec", max = +2)
    updateSliderInput(session, "start_ec", value = val_ec - 0.5)
    updateSliderInput(session, "switch_ec", value = val_ec + 0.5)
    updateNumericInput(session, "concbulk_ec",
                       label = "bulk [Red] (mM)")
  }
})

observe({
  ct_ec = round(abs(2 * (input$start_ec - input$switch_ec))/10^(input$scanrate_ec),digits = 3)
  if(ct_ec > 1){
    updateSliderInput(session, "cursor_ec", max = ceiling(ct_ec), step = ct_ec/100)
  } else {
    updateSliderInput(session, "cursor_ec", max = ct_ec, step = ct_ec/100)
  }
})

observe({
  maxy_ec = input$concbulk_ec
  updateSliderInput(session, "scale_ec", max = maxy_ec)
  updateSliderInput(session, "scale_ec", value = c(0, maxy_ec))
})

# cursor_ec = reactive({
#   x_ec = input$cursor_ec
#   x_ec
# })

cv_ec = reactive({
  out = cvSim(mechanism = "EC", e.start = input$start_ec,
              e.switch = input$switch_ec, e.form = input$formal_ec,
              ko = 10^(input$ko_ec),
              kcf = 10^(input$kcf_ec), kcr = 10^(input$kcr_ec),
              n = as.numeric(input$n_ec), alpha = input$alpha_ec,
              d = input$difcoef_ec * 1e-5, area = input$area_ec,
              scan.rate = 10^(input$scanrate_ec),
              conc.bulk = input$concbulk_ec/1000,
              temp = input$temp_ec, t.units = 2000, x.units = 180,
              sd.noise = 0)
  out
})

output$PvsT_ec = renderPlot({
  old.par = par(mar = c(4.1, 4.1, 1.0, 2.1), mgp = c(2, 1, 0))
  plotPotential(cv_ec(), main_title = "Applied Potential Waveform")
  id = which.min(abs(cv_ec()$time - (input$cursor_ec)))
  points(x = cv_ec()$time[id],
         y = cv_ec()$potential[id],
         pch = 19, cex = 1.5, col = "black")
  par(old.par)

})

output$IvsP_ec = renderPlot({
  old.par = par(mar = c(4.1, 4.1, 1.0, 2.1), mgp = c(2, 1, 0))
  annotateCV(cv_ec())
  title(main = "Cyclic Voltammogram")
  id = which.min(abs(cv_ec()$time - (input$cursor_ec)))
  points(x = cv_ec()$potential[id],
         y = cv_ec()$current[id],
         pch = 19, cex = 1.5, col = "black")
  par(old.par)
})

output$CvsD_ec = renderPlot({
  old.par = par(mar = c(4.1, 4.1, 1.00, 2.1), mgp = c(2, 1, 0))
  plotDiffusion(cv_ec(), t = input$cursor_ec,
                miny = input$scale_ec[1], maxy = input$scale_ec[2])
  par(old.par)
})

#' code for mechanism CE tab; see above for notes on the code

observe({
  val_ce = input$formal_ce
  dir_ce = input$direction_ce
  if (dir_ce == 1) {
    updateSliderInput(session, "start_ce", min = val_ce + 0.25)
    updateSliderInput(session, "start_ce", max = +2)
    updateSliderInput(session, "switch_ce", max = val_ce - 0.25)
    updateSliderInput(session, "switch_ce", min = -2)
    updateSliderInput(session, "start_ce", value = val_ce + 0.5)
    updateSliderInput(session, "switch_ce", value = val_ce - 0.5)
    updateNumericInput(session, "concbulk_ce",
                       label = "bulk [Z + Ox] (mM)")
  } else {
    updateSliderInput(session, "start_ce", min = -2)
    updateSliderInput(session, "start_ce", max = val_ce - 0.25)
    updateSliderInput(session, "switch_ce", min = val_ce +  0.25)
    updateSliderInput(session, "switch_ce", max = +2)
    updateSliderInput(session, "start_ce", value = val_ce - 0.5)
    updateSliderInput(session, "switch_ce", value = val_ce + 0.5)
    updateNumericInput(session, "concbulk_ce",
                       label = "bulk [Z + Red] (mM)")
  }
})

observe({
  ct_ce = round(abs(2 * (input$start_ce - input$switch_ce))/10^(input$scanrate_ce), digits = 3)
  if(ct_ce > 1){
    updateSliderInput(session, "cursor_ce", max = ceiling(ct_ce), step = ct_ce/100)
  } else {
    updateSliderInput(session, "cursor_ce", max = ct_ce, step = ct_ce/100)
  }
})

observe({
  maxy_ce = input$concbulk_ce
  updateSliderInput(session, "scale_ce", max = maxy_ce)
  updateSliderInput(session, "scale_ce", value = c(0, maxy_ce))
})

# cursor_ce = reactive({
#   x_ce = input$cursor_ce
#   x_ce
# })

cv_ce = reactive({
  out = cvSim(mechanism = "CE", e.start = input$start_ce,
              e.switch = input$switch_ce, e.form = input$formal_ce,
              ko = 10^(input$ko_ce),
              kcf = 10^(input$kcf_ce), kcr = 10^(input$kcr_ce),
              n = as.numeric(input$n_ce), alpha = input$alpha_ce,
              d = input$difcoef_ce * 1e-5, area = input$area_ce,
              scan.rate = 10^(input$scanrate_ce),
              conc.bulk = input$concbulk_ce/1000,
              temp = input$temp_ce, t.units = 2000, x.units = 180,
              sd.noise = 0)
  out
})

output$PvsT_ce = renderPlot({
  old.par = par(mar = c(4.1, 4.1, 1.0, 2.1), mgp = c(2, 1, 0))
  plotPotential(cv_ce(), main_title = "Applied Potential Waveform")
  # abline(v = cursor_ce(), lwd = 2, lty = 2, col = "red")
  id = which.min(abs(cv_ce()$time - (input$cursor_ce)))
  points(x = cv_ce()$time[id],
         y = cv_ce()$potential[id],
         pch = 19, cex = 1.5, col = "black")
  par(old.par)

})

output$IvsP_ce = renderPlot({
  old.par = par(mar = c(4.1, 4.1, 1.0, 2.1), mgp = c(2, 1, 0))
  annotateCV(cv_ce())
  title(main = "Cyclic Voltammogram")
  id = which.min(abs(cv_ce()$time - (input$cursor_ce)))
  points(x = cv_ce()$potential[id],
         y = cv_ce()$current[id],
         pch = 19, cex = 1.5, col = "black")
  par(old.par)
})

output$CvsD_ce = renderPlot({
  old.par = par(mar = c(4.1, 4.1, 1.00, 2.1), mgp = c(2, 1, 0))
  plotDiffusion(cv_ce(), t = input$cursor_ce,
                miny = input$scale_ce[1], maxy = input$scale_ce[2])
  par(old.par)
})

} # close server code
) # close shinyServer
