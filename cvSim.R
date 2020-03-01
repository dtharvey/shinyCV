cvSim = function(e.start = 0.0, e.switch = -0.5, e.form = -0.25,
                 mechanism = c("E", "EC", "CE"),
                 ko = 1, kcf = 0, kcr = 0,
                 n = 1, alpha = 0.50, d = 1e-5, area = 0.01, 
                 temp = 298.15, scan.rate = 1.0, conc.bulk = 1e-3,
                 t.units = 1000, x.units = 100, sd.noise = 0) {
  
#' Test to ensure that t.units and x.units satisfy constraint that the number of distance units is less than (18 * number of time unit)^(0.5). The accuracy of the simulation improves with an increase in the number of time units and the number of distance units, but at the cost of an increase in the time needed to calculate the diffusion grid; the default condition of 1000 time units and 100 distance units has a total system time of approximately 0.1 s
  
  if (x.units >= sqrt(18 * t.units)) {
    stop("x.units must be less than sqrt(18 * t.units)")
  }
  
  mechanism = match.arg(mechanism)
  
  if (mechanism == "E") {
    if (kcf != 0 | kcr != 0) {
      stop("For the E mechanism, kcf and kcr must have values of 0.")
    }
  }
  
  if (mechanism == "CE") {
    if (kcf <= 0) {
      stop("For the CE mechanism, kcf must have a value greater than 0.")
    }
  }
  
#' physical constants used in simulations: Faraday's contant (F) in C/mol and the gas constant (R) in J/K•mol
  
  f = 96485   
  r = 8.31451  
  
#' define the limits for the diffusion grid with respect to time and to distance, and calculate additional simulation parameters, including bulk concentrations of all species
  #'  t.tot: time to complete one full sweep from e.start to e.start (s)
  #'  delta.t: increment in time (s)
  #'  time: vector of discrete times for diffusion grid (s)
  #'  x.tot: max distance chosen to exceed difusion limit (cm)
  #'  delta.x: increment in distance (cm)
  #'  distance: vector of discrete distances for diffusion grid (cm)
  #'  lambda: a gathering of constants (unitless)
  #'  direction: -1 for initial reduction and +1 for initial oxidation
  #'  cox.bulk: bulk concentration of Ox (mol/cm^3)
  #'  cred.bulk: bulk concentration of Red (mol/cm^3)
  
  t.tot = 2*abs((e.start - e.switch))/scan.rate
  delta.t = t.tot/t.units
  time = seq(0, t.tot, delta.t)
  x.tot = 6 * sqrt(d * t.tot)
  delta.x = x.tot/x.units
  distance = seq(0, x.tot, delta.x)
  lambda = d * delta.t/(delta.x)^2
  if (e.start > e.switch) {
    direction = -1
    if (mechanism == "CE") {
      cchem.bulk = conc.bulk/(1 + kcf/kcr)
      cox.bulk = conc.bulk - cchem.bulk
      cred.bulk = 0
    } else {
      cox.bulk = conc.bulk
      cred.bulk = 0
      cchem.bulk = 0
    }
  } else {
    direction = +1
    if (mechanism == "CE") {
      cchem.bulk = conc.bulk/(1 + kcf/kcr)
      cox.bulk = 0
      cred.bulk = conc.bulk - cchem.bulk
    } else {
      cox.bulk = 0
      cred.bulk = conc.bulk
      cchem.bulk = 0
    }
  }
  
#' check to ensure that the number of time units satisfies Gosser's requirement for accuracy when using an EC or CE mechanism
  
  if (mechanism != "E") {
    min_tunits = 4 * t.tot * kcf
    if (t.units < min_tunits) {
      stop(paste("Minimum time units is", min_tunits, "if kcf =", kcf, "and with a total scan time of", t.tot, "s."))
    }
  }
  
#' create vector of discrete applied potentials
  
  pot_forward = seq(e.start, e.switch, direction * scan.rate * delta.t)
  pot_reverse = seq(e.switch, e.start, -direction * scan.rate * delta.t)
  potential = c(pot_forward, pot_reverse[-1])
  
# calculate the potential-dependent forward (kf) and reverse (kb) heterogeneous electron-transfer rate constants
  
  kf = ko * exp(-alpha * n * f * (potential - e.form)/(r*temp))
  kb = ko * exp((1 - alpha) * n * f * (potential - e.form)/(r*temp))
  
#' initialize diffusion grids (rows = time; cols = distance) using bulk concentrations for Ox, Red, and Chem and adjusting concentrations to mol/cm^3; the actual concentrations are calculated later
  
  dif.ox = matrix(cox.bulk/1000, nrow = t.units + 1, ncol = x.units + 1)
  dif.red = matrix(cred.bulk/1000, nrow = t.units + 1, ncol = x.units + 1)
  dif.chem = matrix(cchem.bulk/1000, nrow = t.units + 1, ncol = x.units + 1)
  
#' create vectors for fluxes and current, which are calculated later; the initial values here are not important as actual values are calculated later
  
  jox = rep(0, t.units + 1)
  jred = rep(0, t.units + 1)
  current.total = rep(0, t.units + 1)
  
#' calculate diffusion grids over time and, at each time, over distance; for each time the diffusion grids is calculated at all distances except for at the electrode surface; next, for each time, the flux of each species to the electrode surface is used to calculate their concentrations at the electrode surface; and, finally, for each time, the current is calculated
  
  if (mechanism == "CE") {
    for (i in 2:(t.units + 1)){
      for (j in 2:x.units) {
        if (direction == -1) {
          dif.ox[i, j] = dif.ox[i-1, j] + lambda * (dif.ox[i-1, j-1] - 2 * dif.ox[i-1, j] + dif.ox[i-1, j+1]) + kcf * delta.t * dif.chem[i-1, j] - kcr * delta.t * dif.ox[i-1, j]
          dif.red[i, j] = dif.red[i-1, j] + lambda * (dif.red[i-1, j-1] - 2 * dif.red[i-1, j] + dif.red[i-1, j+1]) 
          dif.chem[i, j] = dif.chem[i-1, j] + lambda * (dif.chem[i-1, j-1] - 2 * dif.chem[i-1, j] + dif.chem[i-1, j+1]) - kcf * delta.t * dif.chem[i-1, j] + kcr * delta.t * dif.ox[i-1, j]
        } else {
          dif.ox[i, j] = dif.ox[i-1, j] + lambda * (dif.ox[i-1, j-1] - 2 * dif.ox[i-1, j] + dif.ox[i-1, j+1]) 
          dif.red[i, j] = dif.red[i-1, j] + lambda * (dif.red[i-1, j-1] - 2 * dif.red[i-1, j] + dif.red[i-1, j+1]) + kcf * delta.t*dif.chem[i-1, j] - kcr * delta.t * dif.red[i-1, j]
          dif.chem[i, j] = dif.chem[i-1, j] + lambda * (dif.chem[i-1, j-1] - 2 * dif.chem[i-1, j] + dif.chem[i-1, j+1]) - kcf * delta.t * dif.chem[i-1, j] + kcr * delta.t * dif.red[i-1, j]
        }
      }
      jox[i] = -(kf[i] * dif.ox[i,2] - kb[i] * dif.red[i,2])/(1 + (kf[i] * delta.x)/d + (kb[i] * delta.x)/d)
      jred[i] = -jox[i]
      dif.ox[i, 1] = dif.ox[i, 2] + jox[i] * delta.x/d
      dif.red[i, 1] = dif.red[i, 2] + jred[i] * delta.x/d
      if (direction == -1) {
        dif.chem[i,1] = dif.chem[i, 2]
      } else {
        dif.chem[i,1] = dif.chem[i, 2]
      }
      current.total[i] = -n * f * area * jox[i]
    }
  } else {
    for (i in 2:(t.units + 1)){
      for (j in 2:x.units) {
        if (direction == -1) {
          dif.ox[i, j] = dif.ox[i-1, j] + lambda * (dif.ox[i-1, j-1] - 2 * dif.ox[i-1, j] + dif.ox[i-1, j+1])
          dif.red[i, j] = dif.red[i-1, j] + lambda * (dif.red[i-1, j-1] - 2 * dif.red[i-1, j] + dif.red[i-1, j+1]) - kcf * delta.t * dif.red[i-1, j] + kcr * delta.t * dif.chem[i - 1, j]
          dif.chem[i, j] = dif.chem[i-1, j] + lambda * (dif.chem[i-1, j-1] - 2 * dif.chem[i-1, j] + dif.chem[i-1, j+1]) + kcf * delta.t * dif.red[i-1, j] - kcr * delta.t * dif.chem[i - 1, j]
        } else {
          dif.ox[i, j] = dif.ox[i-1, j] + lambda * (dif.ox[i-1, j-1] - 2 * dif.ox[i-1, j] + dif.ox[i-1, j+1]) - kcf * delta.t * dif.ox[i-1, j] + kcr * delta.t * dif.chem[i - 1, j]
          dif.red[i, j] = dif.red[i-1, j] + lambda * (dif.red[i-1, j-1] - 2 * dif.red[i-1, j] + dif.red[i-1, j+1]) 
          dif.chem[i, j] = dif.chem[i-1, j] + lambda * (dif.chem[i-1, j-1] - 2 * dif.chem[i-1, j] + dif.chem[i-1, j+1]) + kcf * delta.t * dif.ox[i-1, j] - kcr * delta.t * dif.chem[i - 1, j]
        }
      }
      jox[i] = -(kf[i] * dif.ox[i,2] - kb[i] * dif.red[i,2])/(1 + (kf[i] * delta.x)/d + (kb[i] * delta.x)/d)
      jred[i] = -jox[i]
      dif.ox[i, 1] = dif.ox[i, 2] + jox[i] * delta.x/d
      dif.red[i, 1] = dif.red[i, 2] + jred[i] * delta.x/d
      if (direction == -1) {
        dif.chem[i,1] = dif.chem[i, 2]
      } else {
        dif.chem[i,1] = dif.chem[i, 2]
      }
      current.total[i] = -n * f * area * jox[i]
    }
  }
  
#' if desired, add noise to the current; note default is sd.noise = 0, which returns the pure, noise-free simulated cyclic voltammogram
  
  noise = rnorm(t.units + 1, mean = 0, 
                sd = sd.noise * max(abs(current.total))/100)
  current.total = current.total + noise
  
#' return original inputs and calculated results as a list for use with other functions
  
  output = list("expt" = "CV",
                "mechanism" = mechanism,
                "file_type" = "full",
                "current" = current.total*10^6, 
                "potential" = potential,
                "time" = time, 
                "distance" = distance, 
                "oxdata" = dif.ox * 10^6, 
                "reddata" = dif.red * 10^6, 
                "chemdata" = dif.chem * 10^6,
                "formalE" = e.form,
                "initialE" = e.start,
                "switchE" = e.switch,
                "electrons" = n,
                "ko" = ko,
                "kcf" = kcf,
                "kcr" = kcr,
                "alpha" = alpha,
                "diffcoef" = d,
                "area" = area,
                "temperature" = temp,
                "scanrate" = scan.rate,
                "conc.bulk" = conc.bulk,
                "tunits" = t.units,
                "xunits" = x.units,
                "sdnoise" = sd.noise,
                "direction" = direction
  )
  
  invisible(output)
}

plotPotential = function(filename, main_title = NULL){
  plot(x = filename$time, y = filename$potential, lwd = 2, col = "blue",
       type = "l", xlab = "time (sec)", ylab = "potential (V)",
       main = main_title)
  grid()
}

plotDiffusion = function(filename, t, maxy, miny) {
  
  if (t < min(filename$time) | t > max(filename$time)) {
    stop(paste0("Time is limited to a value between ", min(filename$time), " s and ", max(filename$time), " s."))
  }
  
  index = which.min(abs(filename$time - t))
  
  if (filename$mechanism == "E") {
    plot(x = filename$distance, y = filename$oxdata[index, ], 
         type = "l", lwd = 3, 
         col = "blue", ylim = c(miny, maxy),
         xlab = "distance from electrode (cm)", 
         ylab = "concentration (mM)", 
         main = paste0("Diffusion Profiles at ", 
                       round(filename$time[index], digits = 4),
                       " sec & ", 
                       round(filename$potential[index], 
                             digits = 3), " V"))
    lines(x = filename$distance, y = filename$reddata[index, ], 
          lwd = 3, col = "red")
    legend(x = "right", legend = c("Ox", "Red"), 
           fill = c("blue", "red"), border = "white",
           bty = "n", inset = 0.05)
    
    grid()
  } else {
    plot(x = filename$distance, y = filename$oxdata[index, ], 
         type = "l", lwd = 3, 
         col = "blue", ylim = c(miny, maxy),
         xlab = "distance from electrode (cm)", 
         ylab = "concentration (mM)", 
         main = paste0("Diffusion Profiles at ", 
                       round(filename$time[index], digits = 3),
                       " sec & ", 
                       round(filename$potential[index], 
                             digits = 3), " V"))
    lines(x = filename$distance, y = filename$reddata[index, ], 
          lwd = 3, col = "red")
    lines(x = filename$distance, 
          y = filename$chemdata[index, ], 
          lwd = 3, col = "green")
    legend(x = "right", legend = c("Ox", "Red", "Z"), 
           fill = c("blue", "red", "green"), 
           bty = "n", inset = 0.05)
    grid()
  } 
}

annotateCV = function(filename, 
                      forward.per = 5, reverse.per = 5, 
                      threshold = 0.05) {
  
#' plot the cyclic voltammogram
  
  plot(x = filename$potential, 
       y = filename$current, type = "l", col = "blue", lwd = 2,
       xlab = "potential (V)", ylab = "current (µA)", 
       xlim = c(max(filename$potential), min(filename$potential)))
  grid()
  
#' identify Epc and Epa
  
  epc.id = which.max(filename$current)
  epa.id = which.min(filename$current)
  epc = filename$potential[epc.id]
  epa = filename$potential[epa.id]
  
#' create linear models to predict baseline currents using parameters forward.per and reverse.per; lm1 models the baseline for the foward reaction and uses the first forward.per% of the data points to model the baseline and a first-order relationship between current and potential; lm2 models the baseline for the reverse reaction and uses the first reverse.per% of the data points beginning with the switching potential to predict the current decay in the absence of a change in potential using a model in which the current decays as function of t^(-0.5)
  
  n.points1 = (forward.per/100) * filename$tunits
  n.points2 = (reverse.per/100) * filename$tunits
  lm1 = lm(filename$current[1:n.points1] ~ filename$potential[1:n.points1])
  lm2 = lm(filename$current[(filename$tunits/2+1):(filename$tunits/2+n.points2 + 1)] ~ I(filename$time[(filename$tunits/2 + 1):(filename$tunits/2+n.points2 + 1)]^(-0.5)))
  
#' add a dashed line to show the baseline for the forward reaction and add arrow to show the peak current and peak potential
  
  abline(lm1, lwd = 1, lty = 2, col = "black")
  if (filename$direction == -1) {
    arrows(x0 = epc, y0 = lm1$coefficients[1] + lm1$coefficients[2] * filename$potential[epc.id], 
           x1 = epc, y1 = filename$current[epc.id], 
           code = 3, length = 0.1, angle = 15)
  } else {
    arrows(x0 = epa, y0 = lm1$coefficients[1] + lm1$coefficients[2] * filename$potential[epc.id], 
           x1 = epa, y1 = filename$current[epa.id], 
           code = 3, length = 0.1, angle = 15)
  }
  
#' add a dashed line to show the baseline for the reverse reaction and, if threshold conditions are met, add arrow to show the peak current and peak potential
  
  y = lm2$coefficients[1] + lm2$coefficients[2] * filename$time[(filename$tunits/2 + 1):(filename$tunits+1)]^(-0.5)
  lines(filename$potential[(filename$tunits/2+1):(filename$tunits+1)], y,
        lwd = 1, lty = 2, col = "black")
  
#' set flags to control the plotting of annotations; flags for the cathodic and the anodic potentials determine whether potentials are included; flags for the cathodic and the anodic current determine whether currents are included; both sets of flags determine whether arrows are drawn
  
  flag.cpot = FALSE
  flag.ccur = FALSE
  flag.apot = FALSE
  flag.acur = FALSE
  
  if (abs(filename$current[epc.id]) < threshold) {
    flag.cpot = TRUE
    flag.ccur = TRUE
  }
  if (abs(filename$current[epa.id]) < threshold) {
    flag.apot = TRUE
    flag.acur = TRUE
  }
  
  if (filename$direction == -1 & flag.apot == FALSE) {
    if (filename$current[epa.id] - (y[epa.id - filename$tunits/2 + 1]) > 0) {
      flag.acur = TRUE
    }
  }
  if (filename$direction == 1 & flag.cpot == FALSE) {
    if (filename$current[epc.id] - (y[epc.id - filename$tunits/2 + 1]) < 0) {
      flag.ccur = TRUE
    }
  }
  
  if (filename$direction == -1) {
    if (flag.acur == FALSE & flag.apot == FALSE) {
      arrows(x0 = epa, y0 = y[epa.id - filename$tunits/2 + 1],
             x1 = epa, y1 = filename$current[epa.id],
             code = 3, length = 0.1, angle = 15)
    }
  } else {
    if (flag.ccur == FALSE & flag.cpot == FALSE) {
      arrows(x0 = epc, y0 = y[epc.id - filename$tunits/2 + 1],
             x1 = epc, y1 = filename$current[epc.id],
             code = 3, length = 0.1, angle = 15)
    }
  }
  
#' calculate ipc and ipa
  
  if (filename$direction == -1) {
    ipc = filename$current[epc.id] - (lm1$coefficients[1] + lm1$coefficients[2] * filename$potential[epc.id])
    ipa = filename$current[epa.id] - y[epa.id - filename$tunits/2+1]
  } else {
    ipa = (filename$current[epa.id] - (lm1$coefficients[1] + lm1$coefficients[2] * filename$potential[epa.id]))
    ipc = (filename$current[epc.id] - y[epc.id - filename$tunits/2+1])
  }
  
#' add annotations to plot for peak potentials, peak currents, delta E and peak current ratio; values are not shown for the reverse reaction if threshold values are not met
  
  delta.y = max(filename$current) - min(filename$current)
  
#' annotations for cathodic peak potential and current
  
  if (flag.cpot == FALSE) {
    text(x = max(filename$potential), 
         y = max(filename$current),
         substitute(paste(E[pc], ": ", epc, " V"), list(epc = noquote(formatC(epc, format = "f", digits = 3)))), 
         adj = c(0, NA), cex = 0.80)
  } else {
    text(x = max(filename$potential), 
         y = max(filename$current),
         substitute(paste(E[pc], ": not measurable")), 
         adj = c(0, NA), cex = 0.80)
  }
  
  if (flag.ccur == FALSE) {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.80 * delta.y,
         substitute(paste(i[pc], ": ", ipc, " µA"), list(ipc = noquote(formatC(ipc, format = "f", digits = 2)))), 
         adj = c(1, NA), cex = 0.80)
  } else {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.80 * delta.y,
         substitute(paste(i[pc], ": not measurable")), 
         adj = c(1, NA), cex = 0.80)
  }
  
#' add annotations for anodic peak potential and current
  
  if (flag.apot == FALSE) {
    text(x = max(filename$potential), 
         y = max(filename$current) - 0.05 * delta.y,
         substitute(paste(E[pa], ": ", epa, " V"), list(epa = noquote(formatC(epa, format = "f", digits = 3)))), 
         adj = c(0, NA), cex = 0.80)
  } else {
    text(x = max(filename$potential), 
         y = max(filename$current) - 0.05 * delta.y,
         substitute(paste(E[pa], ": not measurable")), 
         adj = c(0, NA), cex = 0.80)
  }
  
  if(flag.acur == FALSE) {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.85 * delta.y,
         substitute(paste(i[pa], ": ", ipa, " µA"), list(ipa = noquote(formatC(ipa, format = "f", digits = 2)))), 
         adj = c(1, NA), cex = 0.80)
  } else {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.85 * delta.y,
         substitute(paste(i[pa], ": not measurable")), 
         adj = c(1, NA), cex = 0.80)
  }
  
#' add annotations for delta E, Eavg, and current ratio
  
  if (flag.cpot == FALSE & flag.apot == FALSE) {
    text(x = max(filename$potential),
         y = max(filename$current) - 0.1 * delta.y,
         substitute(paste(Delta, "E: ", deltae, " V"),
                    list(deltae = noquote(formatC(epa - epc, format = "f", digits = 3)))), 
         adj = c(0, NA), cex = 0.80)
    text(x = max(filename$potential),
         y = max(filename$current) - 0.15 * delta.y,
         substitute(paste(E[avg], ": ", eavg, " V"),
                    list(eavg = noquote(formatC(0.5 * (epa + epc), format = "f", digits = 3)))), 
         adj = c(0, NA), cex = 0.80)
  } else {
    text(x = max(filename$potential),
         y = max(filename$current) - 0.1 * delta.y,
         substitute(paste(Delta, "E: not measurable")),
         adj = c(0, NA), cex = 0.80)
    text(x = max(filename$potential),
         y = max(filename$current) - 0.15 * delta.y,
         substitute(paste(E[avg], ": not measurable")),
         adj = c(0, NA), cex = 0.80)
  }
  
  if (flag.ccur == FALSE & flag.acur == FALSE) {
    if (filename$direction == 1) {
      text(x = min(filename$potential),
           y = max(filename$current) - 0.90 * delta.y,
           substitute(paste("|", i[pc]/i[pa], "|: ", ratio), list(ratio = noquote(formatC(abs(ipc/ipa), format = "f", digits = 2)))), 
           adj = c(1, NA), cex = 0.80)
    } else {
      text(x = min(filename$potential),
           y = max(filename$current) - 0.90 * delta.y,
           substitute(paste("|", i[pa]/i[pc], "|: ", ratio), list(ratio = noquote(formatC(abs(ipa/ipc), format = "f", digits = 2)))), 
           adj = c(1, NA), cex = 0.80)
    }
  } else {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.90 * delta.y,
         substitute(paste("|", i[pc]/i[pa], "|: not measurable")), 
         adj = c(1, NA), cex = 0.80)
  }
  
}
