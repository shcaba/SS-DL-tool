VBGF<-function(Linf, k, t0, ages){ 
   Linf * (1 - exp(-k * (ages - t0))) 
  } 


VBGF.age<-function(Linf,k,t0,lt){ 
    t0 - (log(1 - (lt / Linf)) / k) 
  } 
  

RUN.SS<-function(path,ss.cmd=" -nohess -nox"){ 
  navigate <- paste("cd ", path, sep="")
  if(.Platform[["OS.type"]] == "windows"){os_exe <- "ss3"} 
  if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="x86_64"){os_exe <- "ss3_osx"} 
  if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="aarch64"){os_exe <- "ss3_osx_arm64"} 
  if(R.version[["os"]] == "linux-gnu"){os_exe <- "ss3_linux"}
  
  if(os_exe=="ss3") 
    {
      r4ss::run(path,exe=os_exe,extras=ss.cmd,skipfinished=FALSE,show_in_console = TRUE)
    } else {
    command <- c(paste("cd", path), paste0("chmod +x ./", os_exe), paste0("./", os_exe, " ", ss.cmd)) 
    system(paste(command, collapse=";"),invisible=TRUE)
    }
}  

pngfun <- function(wd, file,w=7,h=7,pt=12){
  file <- file.path(wd, file)
  cat('writing PNG to',file,'\n')
  png(filename=file,
      width=w,height=h,
      units='in',res=300,pointsize=pt)
}

rc <- function(n,alpha=1){
  # a subset of rich.colors by Arni Magnusson from the gregmisc package
  # a.k.a. rich.colors.short, but put directly in this function
  # to try to diagnose problem with transparency on one computer
  x <- seq(0, 1, length = n)
  r <- 1/(1 + exp(20 - 35 * x))
  g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  b <- dnorm(x, 0.25, 0.15)/max(dnorm(x, 0.25, 0.15))
  rgb.m <- matrix(c(r, g, b), ncol = 3)
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha=alpha))
}

doubleNorm24.sel <- function(Sel50,Selpeak,PeakDesc,LtPeakFinal,FinalSel) {
#UPDATED: - input e and f on 0 to 1 scal and transfrom to logit scale
#         - changed bin width in peak2 calculation
#         - updated index of sel when j2 < length(x)
#   - renamed input parameters, cannot have same names as the logistic function
#         - function not handling f < -1000 correctly
          x<-seq(1,Selpeak+Selpeak,1)
          bin_width <- x[2] - x[1]
          
          a<- Selpeak
          b<- -log((max(x)-Selpeak-bin_width)/(PeakDesc-Selpeak-bin_width))
          c<- log(-((Sel50-Selpeak)^2/log(0.5)))
          d<- log(LtPeakFinal)
          e<- -15
          f<- -log((1/(FinalSel+0.000000001)-1))
          
      sel <- rep(NA, length(x))
      startbin <- 1
      peak <- a
      upselex <- exp(c)
      downselex <- exp(d)
      final <- f
      if (e < -1000) {
          j1 <- -1001 - round(e)
          sel[1:j1] <- 1e-06
      }
      if (e >= -1000) {
          j1 <- startbin - 1
          if (e > -999) {
            point1 <- 1/(1 + exp(-e))
            t1min <- exp(-(x[startbin] - peak)^2/upselex)
          }
      }
      if (f < -1000)
          j2 <- -1000 - round(f)
      if (f >= -1000)
          j2 <- length(x)
      peak2 <- peak + bin_width + (0.99 * x[j2] - peak - bin_width)/(1 +
          exp(-b))
      if (f > -999) {
          point2 <- 1/(1 + exp(-final))
          t2min <- exp(-(x[j2] - peak2)^2/downselex)
      }
      t1 <- x - peak
      t2 <- x - peak2
      join1 <- 1/(1 + exp(-(20/(1 + abs(t1))) * t1))
      join2 <- 1/(1 + exp(-(20/(1 + abs(t2))) * t2))
      if (e > -999)
          asc <- point1 + (1 - point1) * (exp(-t1^2/upselex) -
            t1min)/(1 - t1min)
      if (e <= -999)
          asc <- exp(-t1^2/upselex)
      if (f > -999)
          dsc <- 1 + (point2 - 1) * (exp(-t2^2/downselex) -
            1)/(t2min - 1)
      if (f <= -999)
          dsc <- exp(-(t2)^2/downselex)
      idx.seq <- (j1 + 1):j2
      sel[idx.seq] <- asc[idx.seq] * (1 - join1[idx.seq]) + join1[idx.seq] * (1 -
          join2[idx.seq] + dsc[idx.seq] * join2[idx.seq])
      if (startbin > 1 && e >= -1000) {
          sel[1:startbin] <- (x[1:startbin]/x[startbin])^2 *
            sel[startbin]
      }
      if (j2 < length(x))
          sel[(j2 + 1):length(x)] <- sel[j2]
      return(cbind(x,sel))
}

gg_color_hue <- function(n) 
  {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
  }

rbeta.ab <- function(n, m, s, a, b)
{
  # calculate mean of corresponding standard beta dist
  mu.std <- (m-a)/(b-a)

  # calculate parameters of std. beta with mean=mu.std and sd=s
  alpha <- (mu.std^2 - mu.std^3 - mu.std*s^2) / s^2
  beta  <- (mu.std - 2*mu.std^2 + mu.std^3 - s^2 + mu.std*s^2) / s^2

  # generate n draws from standard beta
  b.std <- rbeta(n, alpha, beta)

  # linear transformation from beta(0,1) to beta(a,b)
  b.out <- (b-a)*b.std + a

  return(b.out)
}

#current.year: Year to report output
#mod.names: List the names of the sensitivity runs
#likelihood.out=c(1,1,1): Note which likelihoods are in the model (surveys, lengths, ages)
#Sensi.RE.out="Sensi_RE_out.DMP": #Saved file of relative changes
#CI=0.95:Confidence interval box based on the reference model
#TRP.in=0.4:Target relative abundance value
#LRP.in=0.25: Limit relative abundance value
#sensi_xlab="Sensitivity scenarios" : X-axis label
#ylims.in=c(-1,2,-1,2,-1,2,-1,2,-1,2,-1,2): Y-axis label
#plot.figs=c(1,1,1,1,1,1): Which plots to make/save?
#sensi.type.breaks=NA: vertical breaks that can separate out types of sensitivities
#anno.x=NA: Vertical positioning of the sensitivity types labels
#anno.y=NA: Horizontal positioning of the sensitivity types labels
#anno.lab=NA: Sensitivity types labels


Sensi_plot_horiz <- function(model.summaries,
                          dir = "",
                          current.year,
                          mod.names,
                          likelihood.out = c(1, 1, 1),
                          Sensi.RE.out = "Sensi_RE_out.DMP",
                          CI = 0.95,
                          TRP.in = 0.25,
                          LRP.in = 0.125,
                          sensi_xlab = "Sensitivity scenarios",
                          ylims.in = c(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
                          plot.figs = c(1, 1, 1, 1, 1, 1),
                          sensi.type.breaks = NA,
                          anno.x = NA,
                          anno.y = NA,
                          anno.lab = NA, 
                          header.text=2,
                          horizontal = FALSE) {
# internal function
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

#Create all likelihood names
mod.like.names<-unique(model.summaries$likelihoods_by_fleet$Label)
build.like.tab<-colnames(model.summaries$likelihoods_by_fleet)
Fleet.likes<-model.summaries$likelihoods_by_fleet
mod_sums.temp1<-Fleet.likes[1,]
mod_sums.temp1[,1:length(mod_sums.temp1)]<-NA
for(t in 1:model.summaries$n)
  {
    mod.labs.in<-subset(Fleet.likes,model==t)$Label
    add.labs<-mod.like.names[!mod.like.names%in%mod.labs.in]
    if(length(add.labs)>0)
    {
      mod.likes<-mod_sums.temp1%>% slice(rep(1:n(), each = length(add.labs)))    
      mod.likes[,1]<-t
      mod.likes[,2]<-add.labs
      Fleet.likes<-rbind(Fleet.likes,mod.likes)
    }
  }

# num.likes<-sum(likelihood.out)*2+2
num.likes <- dim(Fleet.likes)[1] # determine how many likelihoods components

if (missing(mod.names)) {
  mod.names <- paste("model ", 1:model.summaries$n)
}

# grab the survey likelihoods
if (likelihood.out[1] == 1 & any(mod.like.names=="Surv_like")) {
  syrvlambda_index <- c(1:num.likes)[Fleet.likes$Label == "Surv_lambda"]
  survey.lambda <- data.frame(rownames(t(Fleet.likes))[-1:-2], t(Fleet.likes[syrvlambda_index, ][-1:-2]), "Survey_lambda")
  syrvlike_index <- c(1:num.likes)[Fleet.likes$Label == "Surv_like"]
  survey.like <- data.frame(rownames(t(Fleet.likes))[-1:-2], t(Fleet.likes[syrvlike_index, ][-1:-2]), "Survey_likelihood")
} else {
  survey.lambda <- survey.like <- data.frame(t(rep(NA, model.summaries$n + 2)))
}

# length likelihoods
if (likelihood.out[2] == 1 & any(mod.like.names=="Length_like")) {
  Ltlambda_index <- c(1:num.likes)[Fleet.likes$Label == "Length_lambda"]
  Lt.lambda <- data.frame(rownames(t(Fleet.likes))[-1:-2], t(Fleet.likes[Ltlambda_index, ][-1:-2]), "Lt_lambda")
  Ltlike_index <- c(1:num.likes)[Fleet.likes$Label == "Length_like"]
  Lt.like <- data.frame(rownames(t(Fleet.likes))[-1:-2], t(Fleet.likes[Ltlike_index, ][-1:-2]), "Lt_likelihood")
} else {
  Lt.lambda <- Lt.like <- data.frame(t(rep(NA, model.summaries$n + 2)))
}

# age likelihood
if (likelihood.out[3] == 1 & any(mod.like.names=="Age_like")) {
  Agelambda_index <- c(1:num.likes)[Fleet.likes$Label == "Age_lambda"]
  Age.lambda <- data.frame(rownames(t(Fleet.likes))[-1:-2], t(Fleet.likes[Agelambda_index, ][-1:-2]), "Age_lambda")
  Agelike_index <- c(1:num.likes)[Fleet.likes$Label == "Age_like"]
  Age.like <- data.frame(rownames(t(Fleet.likes))[-1:-2], t(Fleet.likes[Agelike_index, ][-1:-2]), "Age_likelihood")
} else {
  Age.lambda <- Age.like <- data.frame(t(rep(NA, model.summaries$n + 2)))
}

parms <- model.summaries$pars
# rownames(parms)<-parms$Label
parms <- data.frame(parms$Label, parms[, 1:(dim(parms)[2] - 3)], "Parameters")
if (any(model.summaries$nsexes == 1)) {
  dev.quants <- rbind(
    model.summaries$quants[model.summaries$quants$Label == "SSB_Initial", 1:(dim(model.summaries$quants)[2] - 2)] / 2,
    (model.summaries$quants[model.summaries$quants$Label == paste0("SSB_", current.year), 1:(dim(model.summaries$quants)[2] - 2)]) / 2,
    model.summaries$quants[model.summaries$quants$Label == paste0("Bratio_", current.year), 1:(dim(model.summaries$quants)[2] - 2)],
    model.summaries$quants[model.summaries$quants$Label == "Dead_Catch_SPR", 1:(dim(model.summaries$quants)[2] - 2)] / 2,
    model.summaries$quants[model.summaries$quants$Label == "annF_SPR", 1:(dim(model.summaries$quants)[2] - 2)]
  )
  # Extract SDs for use in the ggplots
  dev.quants.SD <- c(
    model.summaries$quantsSD[model.summaries$quantsSD$Label == "SSB_Initial", 1] / 2,
    (model.summaries$quantsSD[model.summaries$quantsSD$Label == paste0("SSB_", current.year), 1]) / 2,
    model.summaries$quantsSD[model.summaries$quantsSD$Label == paste0("Bratio_", current.year), 1],
    model.summaries$quantsSD[model.summaries$quantsSD$Label == "Dead_Catch_SPR", 1] / 2,
    model.summaries$quantsSD[model.summaries$quantsSD$Label == "annF_SPR", 1]
  )
} # end single-sex check

if (any(model.summaries$nsexes == 2)) {
  dev.quants <- rbind(
    model.summaries$quants[model.summaries$quants$Label == "SSB_Initial", 1:(dim(model.summaries$quants)[2] - 2)],
    model.summaries$quants[model.summaries$quants$Label == paste0("SSB_", current.year), 1:(dim(model.summaries$quants)[2] - 2)],
    model.summaries$quants[model.summaries$quants$Label == paste0("Bratio_", current.year), 1:(dim(model.summaries$quants)[2] - 2)],
    model.summaries$quants[model.summaries$quants$Label == "Dead_Catch_SPR", 1:(dim(model.summaries$quants)[2] - 2)],
    model.summaries$quants[model.summaries$quants$Label == "annF_SPR", 1:(dim(model.summaries$quants)[2] - 2)]
  )
  # Extract SDs for use in the ggplots
  dev.quants.SD <- c(
    model.summaries$quantsSD[model.summaries$quantsSD$Label == "SSB_Initial", 1],
    (model.summaries$quantsSD[model.summaries$quantsSD$Label == paste0("SSB_", current.year), 1]),
    model.summaries$quantsSD[model.summaries$quantsSD$Label == paste0("Bratio_", current.year), 1],
    model.summaries$quantsSD[model.summaries$quantsSD$Label == "Dead_Catch_SPR", 1],
    model.summaries$quantsSD[model.summaries$quantsSD$Label == "annF_SPR", 1]
  )
} # end two-sex check

dev.quants.labs <- data.frame(c("SB0", paste0("SSB_", current.year), 
    paste0("Bratio_", current.year), "MSY_SPR", "F_SPR"), 
    dev.quants, "Derived quantities")
  AICs <- 2 * model.summaries[["npars"]] + (2 * as.numeric(model.summaries[["likelihoods"]][1, 
    1:model.summaries[["n"]]]))
  deltaAICs <- AICs - AICs[1]
  AIC.out <- data.frame(cbind(c("AIC", "deltaAIC"), rbind.data.frame(AICs, deltaAICs), c("AIC")))
  colnames(AIC.out) <- colnames(survey.lambda) <- colnames(survey.like) <- colnames(Lt.lambda) <- colnames(Lt.like) <- colnames(Age.lambda) <- colnames(Age.like) <- colnames(parms) <- colnames(dev.quants.labs) <- 
  c("Type",mod.names, "Label")
  Like.parm.quants <- rbind(AIC.out, survey.like, survey.lambda, 
    Lt.like, Lt.lambda, Age.like, Age.lambda, parms, dev.quants.labs)
  Like.parm.quants.table.data <- flextable::as_grouped_data(Like.parm.quants, 
    groups = c("Label"))
  write.csv(Like.parm.quants.table.data, file.path(dir, "Likes_parms_devquants_table.csv"))
  
# dev.quants.labs <- data.frame(c("SB0", paste0("SSB_", current.year), paste0("Bratio_", current.year), "MSY_SPR", "F_SPR"), dev.quants, "Derived quantities")
# AICs <- 2 * model.summaries$npars + (2 * as.numeric(model.summaries$likelihoods[1, 1:model.summaries$n]))
# deltaAICs <- AICs - AICs[1]
# AIC.out <- data.frame(cbind(c("AIC", "deltaAIC"), rbind.data.frame(AICs, deltaAICs), c("AIC")))
# colnames(AIC.out) <- colnames(survey.lambda) <- colnames(survey.like) <- colnames(Lt.lambda) <- colnames(Lt.like) <- colnames(Age.lambda) <- colnames(Age.like) <- colnames(parms) <- colnames(dev.quants.labs) <- c("Type", mod.names, "Label")
# Like.parm.quants <- rbind(AIC.out, survey.like, survey.lambda, Lt.like, Lt.lambda, Age.like, Age.lambda, parms, dev.quants.labs)
# Like.parm.quants.table.data <- flextable::as_grouped_data(Like.parm.quants, groups = c("Label"))
# # as_flextable(Like.parm.quants.table.data)
# write.csv( Like.parm.quants.table.data, file.path(dir, "Likes_parms_devquants_table.csv")
# )

# Calcualte Relative changes
dev.quants.mat <- as.matrix(dev.quants)
colnames(dev.quants.mat) <- 1:dim(dev.quants.mat)[2]
rownames(dev.quants.mat) <- c("SB0", paste0("SSB_", current.year), paste0("Bratio_", current.year), "MSY_SPR", "F_SPR")
# RE<-reshape2::melt((as.matrix(dev.quants)-as.matrix(dev.quants)[,1])/as.matrix(dev.quants)[,1])
RE <- reshape2::melt((dev.quants.mat - dev.quants.mat[, 1]) / dev.quants.mat[, 1])[-1:-5, ]
logRE <- reshape2::melt(log(dev.quants.mat / dev.quants.mat[, 1]))[-1:-5, ]
# Get values for plots
Dev.quants.temp <- as.data.frame(cbind(rownames(dev.quants.mat), dev.quants.mat[, -1]))
colnames(Dev.quants.temp) <- c("Metric", mod.names[-1])
Dev.quants.ggplot <- data.frame(reshape2::melt(Dev.quants.temp, id.vars = c("Metric")), RE[, 2:3], logRE[, 2:3])
colnames(Dev.quants.ggplot) <- c("Metric", "Model_name", "Value", "Model_num_plot", "RE", "Model_num_plot_log", "logRE")
Dev.quants.ggplot$Metric <- factor(Dev.quants.ggplot$Metric, levels = unique(Dev.quants.ggplot$Metric))
save(Dev.quants.ggplot, file = file.path(dir, Sensi.RE.out))

# Calculate RE values for reference model boxes
CI_DQs_RE <- ((dev.quants[, 1] + dev.quants.SD * qnorm(CI)) - dev.quants[, 1]) / dev.quants[, 1]
TRP <- (TRP.in - dev.quants[3, 1]) / dev.quants[3, 1]
LRP <- (LRP.in - dev.quants[3, 1]) / dev.quants[3, 1]

logCI_DQs_RE <- log((dev.quants[, 1] + dev.quants.SD * qnorm(CI)) / dev.quants[, 1])
logTRP <- log(TRP.in / dev.quants[3, 1])
logLRP <- log(LRP.in / dev.quants[3, 1])

# Plot Relative changes
four.colors <- gg_color_hue(5)
lty.in <- 2
if (any(is.na(sensi.type.breaks))) {
  lty.in <- 0
  sensi.type.breaks <- c(1, 1)
}
if (any(is.na(anno.x))) {
  anno.x <- c(1, 1)
}
if (any(is.na(anno.y))) {
  anno.y <- c(1, 1)
}

if (any(is.na(anno.lab))) {
  anno.lab <- c("", "")
}

SPRtarg.lab<-paste0('SPR',model.summaries[["sprtargs"]][1]*100,"%")

#if (horizontal == TRUE) {
  # RE plot
  ggplot(Dev.quants.ggplot, aes(RE, .data$Model_num_plot)) +
    geom_point(aes(shape = .data$Metric, color = .data$Metric), size = 3) +
    geom_rect(aes(ymin = 1, ymax = model.summaries$n + 1, xmin = -CI_DQs_RE[1], xmax = CI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
    geom_rect(aes(ymin = 1, ymax = model.summaries$n + 1, xmin = -CI_DQs_RE[2], xmax = CI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
    geom_rect(aes(ymin = 1, ymax = model.summaries$n + 1, xmin = -CI_DQs_RE[3], xmax = CI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
    geom_rect(aes(ymin = 1, ymax = model.summaries$n + 1, xmin = -CI_DQs_RE[4], xmax = CI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
    geom_rect(aes(ymin = 1, ymax = model.summaries$n + 1, xmin = -CI_DQs_RE[5], xmax = CI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
    geom_vline(xintercept = c(TRP + 0.03, LRP - 0.03, 0), lty = c(2, 2, 1), color = c("darkgreen", "darkred", "gray")) +
    scale_y_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot$Model_name)) +
    # scale_y_continuous(limits=ylims.in[1:2])+
    coord_cartesian(xlim = ylims.in[1:2]) +
    theme(
      axis.text.y = element_text(size = 14, color = 1),
      axis.text.x = element_text(size = 14, color = 1), 
      legend.text.align = 0,
      axis.title.x = element_text(size = 14),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    )  +
    scale_shape_manual(
      values = c(15:18, 12),
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)])),
        bquote(frac(SB[.(current.year)], SB[0])),
        bquote(Yield[.(SPRtarg.lab)]),
        bquote(F[.(SPRtarg.lab)])
      )
    ) +
    scale_color_manual(
      values = four.colors[1:5],
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)])),
        bquote(frac(SB[.(current.year)], SB[0])),
        bquote(Yield[.(SPRtarg.lab)]),
        bquote(F[.(SPRtarg.lab)])
      )
    ) +
    labs(y = "", x = "Relative change") +
    annotate("text", y = anno.x, x = anno.y, label = anno.lab, size = 5, col = 'grey10') +
    annotate("text", x = c(0, 0), y = c(TRP + 0.03, LRP - 0.03), label = c("TRP", "LRP"), size = c(2, 2), color = c("darkgreen", "darkred")) +
    #annotate("text", y = c((model.summaries$n + 2), (model.summaries$n + 2)), 
      #x = c(TRP + 0.08, LRP - 0.08), label = c("TRP", "LRP"), size = c(5, 5), color = c("darkgreen", "darkred")) +
    geom_hline(yintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_REplot_all_horizontal.png"), width = 12, height = 7)
#}

#if (horizontal == FALSE){
pt.dodge <- 0.3
if (plot.figs[1] == 1) {
   # RE plot
  ggplot(Dev.quants.ggplot, aes(.data$Model_num_plot, RE)) +
    geom_point(aes(shape = .data$Metric, color = .data$Metric), position = position_dodge(pt.dodge)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[1], ymax = CI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[2], ymax = CI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[3], ymax = CI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[4], ymax = CI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[5], ymax = CI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
    geom_hline(yintercept = c(TRP, LRP, 0), lty = c(2, 2, 1), color = c("darkgreen", "darkred", "gray")) +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot$Model_name)) +
    # scale_y_continuous(limits=ylims.in[1:2])+
    coord_cartesian(ylim = ylims.in[1:2]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.text.align = 0,
      panel.grid.minor = element_blank()
    ) +
    scale_shape_manual(
      values = c(15:18, 12),
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)])),
        bquote(frac(SB[.(current.year)], SB[0])),
        bquote(Yield[.(SPRtarg.lab)]),
        bquote(F[.(SPRtarg.lab)])
      )
    ) +
    scale_color_manual(
      values = four.colors[1:5],
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)])),
        bquote(frac(SB[.(current.year)], SB[0])),
        bquote(Yield[.(SPRtarg.lab)]),
        bquote(F[.(SPRtarg.lab)])
      )
    ) +
    labs(x = sensi_xlab, y = "Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    annotate("text", x = c((model.summaries$n + 2), (model.summaries$n + 2)), y = c(TRP + 0.03, LRP - 0.03), label = c("TRP", "LRP"), size = c(3, 3), color = c("darkgreen", "darkred")) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_REplot_all.png"))
  
  # log plot
  ggplot(Dev.quants.ggplot, aes(.data$Model_num_plot, logRE)) +
    geom_point(aes(shape = .data$Metric, color = .data$Metric), position = position_dodge(pt.dodge)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[1], ymax = logCI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[2], ymax = logCI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[3], ymax = logCI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[4], ymax = logCI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[5], ymax = logCI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
    geom_hline(yintercept = c(logTRP, logLRP, 0), lty = c(2, 2, 1), color = c("darkgreen", "darkred", "gray")) +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot$Model_name)) +
    # scale_y_continuous(limits=ylims.in[1:2])+
    coord_cartesian(ylim = ylims.in[1:2]) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.text.align = 0, panel.grid.minor = element_blank()) +
    scale_shape_manual(
      values = c(15:18, 12),
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)])),
        bquote(frac(SB[.(current.year)], SB[0])),
        bquote(Yield[.(SPRtarg.lab)]),
        bquote(F[.(SPRtarg.lab)])
      )
    ) +
    scale_color_manual(
      values = four.colors[1:5],
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)])),
        bquote(frac(SB[.(current.year)], SB[0])),
        bquote(Yield[.(SPRtarg.lab)]),
        bquote(F[.(SPRtarg.lab)])
      )
    ) +
    labs(x = sensi_xlab, y = "Log relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    annotate("text", x = c(0, 0), y = c(logTRP + 0.03, logLRP - 0.03), label = c("TRP", "LRP"), size = c(2, 2), color = c("darkgreen", "darkred")) +
    #annotate("text", x = c((model.summaries$n + 2), (model.summaries$n + 2)), y = c(logTRP + 0.03, logLRP - 0.03), label = c("TRP", "LRP"), size = c(3, 3), color = c("darkgreen", "darkred")) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_logREplot_all.png"))
}

if (plot.figs[1] == 1) {
  # RE plots
  Dev.quants.ggplot.SBs <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[1] | Metric == unique(Dev.quants.ggplot$Metric)[2])
  p1 <- ggplot(Dev.quants.ggplot.SBs, aes(.data$Model_num_plot, RE)) +
    geom_point(aes(shape = .data$Metric, color = .data$Metric), position = position_dodge(pt.dodge)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[1], ymax = CI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[2], ymax = CI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
    scale_x_continuous(breaks = 2:(model.summaries$n)) +
    # scale_y_continuous(limits=ylims.in[1:2])+
    coord_cartesian(ylim = ylims.in[1:2]) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_shape_manual(
      values = c(16, 17),
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)]))
      )
    ) +
    scale_color_manual(
      values = four.colors[1:2],
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)]))
      )
    ) +
    theme(legend.text.align = 0) +
    labs(x = " ", y = " ") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab, size=header.text) +
    geom_hline(yintercept = 0, lwd = 0.5, color = "gray") +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  
  Dev.quants.ggplot.Dep <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[3])
  
  p2 <- ggplot(Dev.quants.ggplot.Dep, aes(.data$Model_num_plot, RE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[3], ymax = CI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
    scale_x_continuous(breaks = 2:(model.summaries$n)) +
    # scale_y_continuous(limits=ylims.in[7:8])+
    coord_cartesian(ylim = ylims.in[7:8]) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(legend.text.align = 0) +
    labs(x = " ", y = "Relative change") +
    scale_colour_manual(
      values = four.colors[3],
      name = "",
      labels = as.expression(bquote(frac(SB[.(current.year)], SB[0])))
    ) +
    annotate("text", x = c(0, 0), y = c(logTRP + 0.03, logLRP - 0.03), label = c("TRP", "LRP"), size = c(2, 2), color = c("darkgreen", "darkred")) +
    #annotate("text", x = c((model.summaries$n + 1), (model.summaries$n + 1)), y = c(TRP + 0.1, LRP - 0.1), label = c("TRP", "LRP"), size = c(3, 3), color = c("darkgreen", "darkred")) +
    geom_hline(yintercept = c(TRP, LRP, 0), lty = c(3, 3, 1), lwd = c(0.5, 0.5, 0.5), color = c("darkgreen", "darkred", "gray")) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  
  Dev.quants.ggplot.MSY_FMSY <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[4] | Metric == unique(Dev.quants.ggplot$Metric)[5])
  p3 <- ggplot(Dev.quants.ggplot.MSY_FMSY, aes(.data$Model_num_plot, RE, group = .data$Metric)) +
    geom_point(aes(shape = .data$Metric, color = .data$Metric), position = position_dodge(pt.dodge)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[4], ymax = CI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[5], ymax = CI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot$Model_name)) +
    # scale_y_continuous(limits=ylims.in[9:10])+
    coord_cartesian(ylim = ylims.in[9:10]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.text.align = 0,
      panel.grid.minor = element_blank(),
      legend.margin=margin(0,0,0,0),
      legend.box.spacing = unit(0, "pt")
    ) +
    #          legend.text=element_text(size=rel(1)))+
    scale_shape_manual(
      values = c(16, 17),
      name = "",
      labels = c(bquote(Yield[.(SPRtarg.lab)]), bquote(F[.(SPRtarg.lab)]))
    ) +
    scale_color_manual(
      values = four.colors[4:5],
      name = "",
      labels = c(bquote(Yield[.(SPRtarg.lab)]), bquote(F[.(SPRtarg.lab)]))
    ) +
    labs(x = sensi_xlab, y = "") +
    guides(fill = FALSE) +
    # annotate("text",x=anno.x,y=anno.y,label=anno.lab)+
    geom_hline(yintercept = 0, lwd = 0.5, color = "gray") +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  
  # p4<-grid.arrange(p1,p2,p3,heights=c(5,5,8))
  p4 <- ggpubr::ggarrange(p1, p2, p3, nrow = 3, ncol = 1, align = "v", heights = c(5, 5, 8))
  ggsave(file.path(dir, "Sensi_REplot_SB_Dep_F_Yield.png"), p4)
  
  # Log plots
  Dev.quants.ggplot.SBs <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[1] | Metric == unique(Dev.quants.ggplot$Metric)[2])
  p1 <- ggplot(Dev.quants.ggplot.SBs, aes(.data$Model_num_plot, logRE)) +
    geom_point(aes(shape = .data$Metric, color = .data$Metric), position = position_dodge(pt.dodge)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[1], ymax = logCI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[2], ymax = logCI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
    scale_x_continuous(breaks = 2:(model.summaries$n)) +
    # scale_y_continuous(limits=ylims.in[1:2])+
    coord_cartesian(ylim = ylims.in[1:2]) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_shape_manual(
      values = c(16, 17),
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)]))
      )
    ) +
    scale_color_manual(
      values = four.colors[1:2],
      name = "",
      labels = c(
        expression(SB[0]),
        as.expression(bquote("SB"[.(current.year)]))
      )
    ) +
    theme(legend.text.align = 0) +
    labs(x = " ", y = " ") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab, size=header.text) +
    geom_hline(yintercept = 0, lwd = 0.5, color = "gray") +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  
  Dev.quants.ggplot.Dep <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[3])
  p2 <- ggplot(Dev.quants.ggplot.Dep, aes(.data$Model_num_plot, logRE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[3], ymax = logCI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
    scale_x_continuous(breaks = 2:(model.summaries$n)) +
    # scale_y_continuous(limits=ylims.in[7:8])+
    coord_cartesian(ylim = ylims.in[7:8]) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(legend.text.align = 0) +
    labs(x = " ", y = "Log relative change") +
    scale_colour_manual(
      values = four.colors[3],
      name = "",
      labels = as.expression(bquote(frac(SB[.(current.year)], SB[0])))
    ) +
    annotate("text", x = c(0, 0), y = c(logTRP + 0.03, logLRP - 0.03), label = c("TRP", "LRP"), size = c(2, 2), color = c("darkgreen", "darkred")) +
    #annotate("text", x = c((model.summaries$n + 1), (model.summaries$n + 1)), y = c(logTRP + 0.08, logLRP - 0.08), label = c("TRP", "LRP"), size = c(3, 3), color = c("darkgreen", "darkred")) +
    geom_hline(yintercept = c(logTRP, logLRP, 0), lty = c(3, 3, 1), lwd = c(0.5, 0.5, 0.5), color = c("darkgreen", "darkred", "gray")) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  
  Dev.quants.ggplot.MSY_FMSY <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[4] | Metric == unique(Dev.quants.ggplot$Metric)[5])
  p3 <- ggplot(Dev.quants.ggplot.MSY_FMSY, aes(.data$Model_num_plot, logRE, group = .data$Metric)) +
    geom_point(aes(shape = .data$Metric, color = .data$Metric), position = position_dodge(pt.dodge)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[4], ymax = logCI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[5], ymax = logCI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot$Model_name)) +
    # scale_y_continuous(limits=ylims.in[9:10])+
    coord_cartesian(ylim = ylims.in[9:10]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.text.align = 0,
      panel.grid.minor = element_blank(),
      legend.margin=margin(0,0,0,0),
      legend.box.spacing = unit(0, "pt")
    ) +
    #          legend.text=element_text(size=7.5))+
    scale_shape_manual(
      values = c(16, 17),
      name = "",
      labels = c(bquote(Yield[.(SPRtarg.lab)]), bquote(F[.(SPRtarg.lab)]))
    ) +
    scale_color_manual(
      values = four.colors[4:5],
      name = "",
      labels = c(bquote(Yield[.(SPRtarg.lab)]), bquote(F[.(SPRtarg.lab)]))
    ) +
    labs(x = sensi_xlab, y = "") +
    guides(fill = FALSE) +
    # annotate("text",x=anno.x,y=anno.y,label=anno.lab)+
    geom_hline(yintercept = 0, lwd = 0.5, color = "gray") +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  
  p4 <- ggpubr::ggarrange(p1, p2, p3, nrow = 3, ncol = 1, align = "v", heights = c(5, 5, 8))
  # p4<-grid.arrange(p1,p2,p3,heights=c(5,5,8))
  ggsave(file.path(dir, "Sensi_logREplot_SB_Dep_F_Yield.png"), p4)
}

if (plot.figs[2] == 1) {
  # RE plot
  Dev.quants.ggplot.SB0 <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[1])
  ggplot(Dev.quants.ggplot.SB0, aes(.data$Model_num_plot, RE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[1], ymax = CI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
    geom_hline(yintercept = 0, lty = 1, color = "gray") +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot.SB0$Model_name)) +
    # scale_y_continuous(limits=ylims.in[3:4])+
    coord_cartesian(ylim = ylims.in[3:4]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.text.align = 0,
      panel.grid.minor = element_blank()
    ) +
    scale_colour_manual(
      values = four.colors[1],
      name = "",
      labels = expression(SB[0])
    ) +
    labs(x = sensi_xlab, y = "Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_REplot_SO_0.png"))
  
  # Log plot
  Dev.quants.ggplot.SB0 <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[1])
  ggplot(Dev.quants.ggplot.SB0, aes(.data$Model_num_plot, logRE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[1], ymax = logCI_DQs_RE[1]), fill = NA, color = four.colors[1]) +
    geom_hline(yintercept = 0, lty = 1, color = "gray") +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot.SB0$Model_name)) +
    # scale_y_continuous(limits=ylims.in[3:4])+
    coord_cartesian(ylim = ylims.in[3:4]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.text.align = 0,
      panel.grid.minor = element_blank()
    ) +
    scale_colour_manual(
      values = four.colors[1],
      name = "",
      labels = expression(SB[0])
    ) +
    labs(x = sensi_xlab, y = "Log Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_logREplot_SO_0.png"))
}

if (plot.figs[3] == 1) {
  # RE plots
  Dev.quants.ggplot.SBt <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[2])
  ggplot(Dev.quants.ggplot.SBt, aes(.data$Model_num_plot, RE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[2], ymax = CI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
    geom_hline(yintercept = 0, lty = 1, color = "gray") +
    scale_x_continuous(breaks = 2:(model.summaries$n), minor_breaks = NULL, labels = unique(Dev.quants.ggplot.SBt$Model_name)) +
    # scale_y_continuous(limits=ylims.in[5:6])+
    coord_cartesian(ylim = ylims.in[5:6]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      # panel.grid.minor = element_blank(),
      legend.text.align = 0
    ) +
    scale_colour_manual(
      values = four.colors[2],
      name = "",
      labels = as.expression(bquote("SB"[.(current.year)]))
    ) +
    labs(x = sensi_xlab, y = "Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_REplot_SOcurrent.png"))
  
  # Log plots
  Dev.quants.ggplot.SBt <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[2])
  ggplot(Dev.quants.ggplot.SBt, aes(.data$Model_num_plot, logRE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[2], ymax = logCI_DQs_RE[2]), fill = NA, color = four.colors[2]) +
    geom_hline(yintercept = 0, lty = 1, color = "gray") +
    scale_x_continuous(breaks = 2:(model.summaries$n), minor_breaks = NULL, labels = unique(Dev.quants.ggplot.SBt$Model_name)) +
    # scale_y_continuous(limits=ylims.in[5:6])+
    coord_cartesian(ylim = ylims.in[5:6]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      # panel.grid.minor = element_blank(),
      legend.text.align = 0
    ) +
    scale_colour_manual(
      values = four.colors[2],
      name = "",
      labels = as.expression(bquote("SB"[.(current.year)]))
    ) +
    labs(x = sensi_xlab, y = "Log Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_logREplot_SOcurrent.png"))
}

if (plot.figs[4] == 1) {
  # RE plots
  Dev.quants.ggplot.Dep <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[3])
  ggplot(Dev.quants.ggplot.Dep, aes(.data$Model_num_plot, RE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[3], ymax = CI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot.Dep$Model_name)) +
    # scale_y_continuous(limits=ylims.in[7:8])+
    coord_cartesian(ylim = ylims.in[7:8]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.text.align = 0,
      panel.grid.minor = element_blank()
    ) +
    labs(x = " ", y = "Relative change") +
    scale_colour_manual(
      values = four.colors[3],
      name = "",
      labels = as.expression(bquote(frac(SB[.(current.year)], SB[0])))
    ) +
    annotate("text", x = c(0, 0), y = c(logTRP + 0.03, logLRP - 0.03), label = c("TRP", "LRP"), size = c(2, 2), color = c("darkgreen", "darkred")) +
    #annotate("text", x = c((model.summaries$n + 2), (model.summaries$n + 2)), y = c(TRP + 0.03, LRP - 0.03), label = c("TRP", "LRP"), size = c(3, 3), color = c("darkgreen", "darkred")) +
    labs(x = sensi_xlab, y = "Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_hline(yintercept = c(TRP, LRP, 0), lty = c(3, 3, 1), lwd = c(0.5, 0.5, 0.5), color = c("darkgreen", "darkred", "gray")) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_REplot_status.png"))
  
  # Log plots
  Dev.quants.ggplot.Dep <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[3])
  ggplot(Dev.quants.ggplot.Dep, aes(.data$Model_num_plot, logRE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[3], ymax = logCI_DQs_RE[3]), fill = NA, color = four.colors[3]) +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot.Dep$Model_name)) +
    # scale_y_continuous(limits=ylims.in[7:8])+
    coord_cartesian(ylim = ylims.in[7:8]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.text.align = 0,
      panel.grid.minor = element_blank()
    ) +
    labs(x = " ", y = "Relative change") +
    scale_colour_manual(
      values = four.colors[3],
      name = "",
      labels = as.expression(bquote(frac(SB[.(current.year)], SB[0])))
    ) +
    annotate("text", x = c(0, 0), y = c(logTRP + 0.03, logLRP - 0.03), label = c("TRP", "LRP"), size = c(2, 2), color = c("darkgreen", "darkred")) +
    #annotate("text", x = c((model.summaries$n + 2), (model.summaries$n + 2)), y = c(logTRP + 0.03, logLRP - 0.03), label = c("TRP", "LRP"), size = c(2, 2), color = c("darkgreen", "darkred")) +
    labs(x = sensi_xlab, y = "Log Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_hline(yintercept = c(logTRP, logLRP, 0), lty = c(3, 3, 1), lwd = c(0.5, 0.5, 0.5), color = c("darkgreen", "darkred", "gray")) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_logREplot_status.png"))
}

if (plot.figs[5] == 1) {
  # RE plots
  Dev.quants.ggplot.MSY <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[4])
  ggplot(Dev.quants.ggplot.MSY, aes(.data$Model_num_plot, RE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[4], ymax = CI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
    geom_hline(yintercept = 0, lty = 1, color = "gray") +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot.MSY$Model_name)) +
    # scale_y_continuous(limits=ylims.in[9:10])+
    coord_cartesian(ylim = ylims.in[9:10]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_color_manual(
      values = four.colors[4],
      name = "",
      labels = as.expression(bquote(Yield[.(SPRtarg.lab)]))
    ) +
    labs(x = sensi_xlab, y = "Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_REplot_Yield.png"))
  # Log plots
  Dev.quants.ggplot.MSY <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[4])
  ggplot(Dev.quants.ggplot.MSY, aes(.data$Model_num_plot, logRE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[4], ymax = logCI_DQs_RE[4]), fill = NA, color = four.colors[4]) +
    geom_hline(yintercept = 0, lty = 1, color = "gray") +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot.MSY$Model_name)) +
    # scale_y_continuous(limits=ylims.in[9:10])+
    coord_cartesian(ylim = ylims.in[9:10]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_color_manual(
      values = four.colors[4],
      name = "",
      labels = as.expression(bquote(Yield[.(SPRtarg.lab)]))
    ) +
    labs(x = sensi_xlab, y = "Log Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_logREplot_Yield.png"))
}

if (plot.figs[6] == 1) {
  # RE plots
  Dev.quants.ggplot.FMSY <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[5])
  ggplot(Dev.quants.ggplot.FMSY, aes(.data$Model_num_plot, RE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -CI_DQs_RE[5], ymax = CI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
    geom_hline(yintercept = 0, lty = 1, color = "gray") +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot.FMSY$Model_name)) +
    # scale_y_continuous(limits=ylims.in[11:12])+
    coord_cartesian(ylim = ylims.in[11:12]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_color_manual(
      values = four.colors[5],
      name = "",
      labels = as.expression(bquote(F[.(SPRtarg.lab)]))
    ) +
    labs(x = sensi_xlab, y = "Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_REplot_FSPR.png"))
  
  # RE plots
  Dev.quants.ggplot.FMSY <- subset(Dev.quants.ggplot, Metric == unique(Dev.quants.ggplot$Metric)[5])
  ggplot(Dev.quants.ggplot.FMSY, aes(.data$Model_num_plot, logRE)) +
    geom_point(aes(color = .data$Metric)) +
    geom_rect(aes(xmin = 1, xmax = model.summaries$n + 1, ymin = -logCI_DQs_RE[5], ymax = logCI_DQs_RE[5]), fill = NA, color = four.colors[5]) +
    geom_hline(yintercept = 0, lty = 1, color = "gray") +
    scale_x_continuous(breaks = 2:(model.summaries$n), labels = unique(Dev.quants.ggplot.FMSY$Model_name)) +
    # scale_y_continuous(limits=ylims.in[11:12])+
    coord_cartesian(ylim = ylims.in[11:12]) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_color_manual(
      values = four.colors[5],
      name = "",
      labels = as.expression(bquote(F[.(SPRtarg.lab)]))
    ) +
    labs(x = sensi_xlab, y = "Log Relative change") +
    annotate("text", x = anno.x, y = anno.y, label = anno.lab) +
    geom_vline(xintercept = c(sensi.type.breaks), lty = lty.in)
  ggsave(file.path(dir, "Sensi_logREplot_FSPR.png"))
}
#}

}

###############################
### Multi-parameter profile ###
###############################

profile_multi<-function (dir, oldctlfile = "control.ss_new", masterctlfile = lifecycle::deprecated(), 
  newctlfile = "control_modified.ss", linenum = NULL, string = NULL, 
  profilevec = NULL, usepar = FALSE, globalpar = FALSE, parlinenum = NULL, 
  parstring = NULL, saveoutput = TRUE, overwrite = TRUE, whichruns = NULL, 
  prior_check = TRUE, read_like = TRUE, exe = "ss3", verbose = TRUE, 
  ...) 
{
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  if (lifecycle::is_present(masterctlfile)) {
    lifecycle::deprecate_warn(when = "1.46.0", what = "profile(masterctlfile)", 
      with = "profile(oldctlfile)")
    oldctlfile <- masterctlfile
  }
  check_exe(exe = exe, dir = dir, verbose = verbose)
  exe_par <- gsub("_.*","",exe)
  if (is.null(linenum) & is.null(string)) {
    stop("You should input either 'linenum' or 'string' (but not both)")
  }
  if (!is.null(linenum) & !is.null(string)) {
    stop("You should input either 'linenum' or 'string' (but not both)")
  }
  if (usepar) {
    if (is.null(parlinenum) & is.null(parstring)) {
      stop("Using par file. You should input either 'parlinenum' or ", 
        "'parstring' (but not both)")
    }
    if (!is.null(parlinenum) & !is.null(parstring)) {
      stop("Using par file. You should input either 'parlinenum' or ", 
        "'parstring' (but not both)")
    }
  }
  if (!is.null(linenum)) {
    npars <- length(linenum)
  }
  if (!is.null(string)) {
    npars <- length(string)
  }
  if (usepar) {
    if (!is.null(parlinenum)) {
      npars <- length(parlinenum)
    }
    if (!is.null(parstring)) {
      npars <- length(parstring)
    }
  }
  if (is.na(npars) || npars < 1) {
    stop("Problem with the number of parameters to profile over. npars = ", 
      npars)
  }
  if (is.null(profilevec)) {
    stop("Missing input 'profilevec'")
  }
  if (npars == 1) {
    n <- length(profilevec)
  }
  else {
    if ((!is.data.frame(profilevec) & !is.matrix(profilevec)) || 
      ncol(profilevec) != npars) {
      stop("'profilevec' should be a data.frame or a matrix with ", 
        npars, " columns")
    }
    n <- length(profilevec[[1]])
    if (any(unlist(lapply(profilevec, FUN = length)) != 
      n)) {
      stop("Each element in the 'profilevec' list should have length ", 
        n)
    }
    if (verbose) {
      if (!is.null(string)) {
        profilevec_df <- data.frame(profilevec)
        names(profilevec_df) <- string
        message("Profiling over ", npars, " parameters\n", 
          paste0(profilevec_df, collapse = "\n"))
      }
    }
  }
  if (is.null(whichruns)) {
    whichruns <- 1:n
  }
  else {
    if (!all(whichruns %in% 1:n)) {
      stop("input whichruns should be NULL or a subset of 1:", 
        n, "\n", sep = "")
    }
  }
  if (verbose) {
    message("Doing runs: ", paste(whichruns, collapse = ", "), 
      ",\n  out of n = ", n)
  }
  converged <- rep(NA, n)
  totallike <- rep(NA, n)
  liketable <- NULL
  if (verbose) {
    message("Changing working directory to ", dir, ",\n", 
      " but will be changed back on exit from function.")
  }
  setwd(dir)
  stdfile <- file.path(dir, "ss.std")
  starter.file <- dir()[tolower(dir()) == "starter.ss"]
  if (length(starter.file) == 0) {
    stop("starter.ss not found in", dir)
  }
  starter <- SS_readstarter(starter.file, verbose = FALSE)
  if (starter[["ctlfile"]] != newctlfile) {
    stop("starter file should be changed to change\n", "'", 
      starter[["ctlfile"]], "' to '", newctlfile, "'")
  }
  if (prior_check & starter[["prior_like"]] == 0) {
    stop("for likelihood profile, you should change the starter file value of\n", 
      " 'Include prior likelihood for non-estimated parameters'\n", 
      " from 0 to 1 and re-run the estimation.\n")
  }
  if (usepar & starter[["init_values_src"]] == 0) {
    stop("With setting 'usepar=TRUE', change the starter file value", 
      " for initial value source from 0 (ctl file) to 1 (par file).\n")
  }
  if (!usepar & starter[["init_values_src"]] == 1) {
    stop("Change the starter file value for initial value source", 
      " from 1 (par file) to 0 (par file) or change to", 
      " profile(..., usepar = TRUE).")
  }
  if (usepar) {
    file.copy(paste(exe_par,".par"), "parfile_original_backup.sso")
  }
  for (i in whichruns) {
    newrepfile <- paste("Report", i, ".sso", sep = "")
    if (!overwrite & file.exists(newrepfile)) {
      message("skipping profile i=", i, "/", n, " because overwrite=FALSE\n", 
        "  and file exists: ", newrepfile)
    }
    else {
      message("running profile i=", i, "/", n)
      if (npars == 1) {
        newvals <- profilevec[i]
      }
      else {
        newvals <- as.numeric(profilevec[i, ])
      }
      SS_changepars(dir = NULL, ctlfile = oldctlfile, 
        newctlfile = newctlfile, linenums = linenum, 
        strings = string, newvals = newvals, estimate = FALSE, 
        verbose = TRUE, repeat.vals = TRUE)
      ctltable_new <- SS_parlines(ctlfile = newctlfile)
      if (!any(ctltable_new[["PHASE"]] == 1)) {
        warning("At least one parameter needs to be estimated in phase 1.\n", 
          "Edit control file to add a parameter\n", 
          "which isn't being profiled over to phase 1.")
      }
      if (usepar) {
        if (globalpar) {
          par <- readLines("parfile_original_backup.sso")
        }
        else {
          par <- readLines(paste0(exe_par,".par"))
        }
        for (ipar in 1:npars) {
          if (!is.null(parstring)) {
            parlinenum <- grep(parstring[ipar], par, 
              fixed = TRUE) + 1
          }
          if (length(parlinenum) == 0) {
            stop("Problem with input parstring = '", 
              parstring[ipar], "'")
          }
          parline <- par[parlinenum[ipar]]
          parval <- as.numeric(parline)
          if (is.na(parval)) {
            stop("Problem with parlinenum or parstring for par file.\n", 
              "line as read: ", parline)
          }
          par[parlinenum[ipar]] <- ifelse(npars > 1, 
            profilevec[i, ipar], profilevec[i])
        }
        note <- c(paste("# New par file created by profile() with the value on line number", 
          linenum), paste("# changed from", parval, 
          "to", profilevec[i]))
        par <- c(par, "#", note)
        message(paste0(note, collapse = "\n"))
        writeLines(par, paste0("ss_input_par", i, ".ss"))
        writeLines(par, paste0(exe_par,".par"))
      }
      if (file.exists(stdfile)) {
        file.remove(stdfile)
      }
      if (file.exists("Report.sso")) {
        file.remove("Report.sso")
      }
      r4ss::run(dir = dir, verbose = verbose, exe = exe, ...)
      converged[i] <- file.exists(stdfile)
      onegood <- FALSE
      if (read_like && file.exists("Report.sso") & file.info("Report.sso")$size > 
        0) {
        onegood <- TRUE
        Rep <- readLines("Report.sso", n = 400)
        skip <- grep("LIKELIHOOD", Rep)[2]
        nrows <- grep("Crash_Pen", Rep) - skip - 1
        like <- read.table("Report.sso", skip = skip, 
          nrows = nrows, header = TRUE, fill = TRUE)
        liketable <- rbind(liketable, as.numeric(like[["logL.Lambda"]]))
      }
      else {
        liketable <- rbind(liketable, rep(NA, 10))
      }
      if (saveoutput) {
        file.copy("Report.sso", paste("Report", i, ".sso", 
          sep = ""), overwrite = overwrite)
        file.copy("CompReport.sso", paste("CompReport", 
          i, ".sso", sep = ""), overwrite = overwrite)
        file.copy("covar.sso", paste("covar", i, ".sso", 
          sep = ""), overwrite = overwrite)
        file.copy("warning.sso", paste("warning", i, 
          ".sso", sep = ""), overwrite = overwrite)
        file.copy("admodel.hes", paste("admodel", i, 
          ".hes", sep = ""), overwrite = overwrite)
        file.copy(paste0(exe_par,".par"), paste(paste0(exe_par,".par_"), i, ".sso", 
          sep = ""), overwrite = overwrite)
      }
    }
  }
  
  if (onegood) {
    liketable <- as.data.frame(liketable)
    names(liketable) <- like[["Component"]]
    bigtable <- cbind(profilevec[whichruns,], converged[whichruns], 
      liketable)
    names(bigtable)[1] <- "Value"
    return(bigtable)
  }
  else {
    stop("Error: no good Report.sso files created in profile")
  }
}

