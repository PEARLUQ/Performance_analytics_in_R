#---------------------------------------------------------------------
#     Illustration for visualization of performance indicators      #
#    R code using public hospitals data in Queensland, Australia    #
#---------------------------------------------------------------------
rm(list = ls())

# Load data
data <-
  read.csv(file = 'QLDaggre.csv',
           na.strings = c("#N/A", "np."),
           as.is = TRUE)

# Processing variables
data$YEAR[data$YEAR==2013] = "FY 2012/2013"
data$YEAR[data$YEAR==2014] = "FY 2013/2014"
data$YEAR[data$YEAR==2015] = "FY 2014/2015"
data$YEAR[data$YEAR==2016] = "FY 2015/2016"
data$NetworkID <- as.character(data$NetworkID)

TEACH = as.character(data$TEACH)
TEACH[TEACH ==1] = "TEACHING"
TEACH[TEACH ==0] = "NON-TEACHING"

REMOTE = rep("NON-REMOTE",nrow(data))
REMOTE[data$LocationCode ==4|data$LocationCode ==5] = "REMOTE"

SIZE = rep("SMALL",nrow(data))
SIZE[data$PeerCode ==5|data$PeerCode ==6] = "VSMALL"
SIZE[data$PeerCode ==1|data$PeerCode ==2] = "LARGE"

lWEPS = log(data$WEPS)

data <- cbind(data[,-c(2,5:16,20:23)],TEACH,REMOTE,SIZE,lWEPS)
#------------------------ Single-Factor KPI ------------------------#
# 1. Barplot for characteristics
  require("ggplot2")
.df <- data.frame(
  x = data$SIZE,
  z = data$TEACH,
  s = data$REMOTE,
  t = data$YEAR
  )
.df <- as.data.frame(with(.df, table(x, z, s, t)))
  # Count number of observations by each combination of features
.plot <- ggplot(data = .df, aes(x = x, y = Freq, fill = z)) +
  geom_bar(width = 0.9,
           position = "stack",
           stat = "identity") +
  scale_y_continuous(expand = c(0.01, 0)) +
  facet_grid(s ~ t) +
  xlab("SIZE") +
  ylab("Number of hospitals") +
  labs(fill = "TEACH") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14,
                                      base_family = "sans") +
  theme(panel.spacing = unit(0.3, "lines"),
        legend.position = "right")
  print(.plot)
  rm(.df, .plot)

# 2. Scatter plot of inpatient days to outpatient visits
  require("ggplot2")
  require("ggExtra")
  
  # Disable scientific notation
  options(scipen=1000)
.df <- data.frame(x = data$DAYS,
                  y = data$OUT,
                  z = data$YEAR)

# 创建数据点用于标签
Yearlabel <- .df %>%
  group_by(z) %>%
  slice(which.max(y))  # 选择每个分组的最大值所在的行

.plot <-
  ggplot(data = .df, aes(
    x = x,
    y = y,
    colour = z,
    shape = z)) +
  geom_point(size = 2.5) +
  # Non-parametric regression
  stat_smooth(aes(fill = z, group = z), method = "loess", alpha = 0.1, size = 1) + # 改变线粗细，改变透明度
  xlab("Number of inpatient days") +
  ylab("Number of outpatients") +
  labs(colour = "YEAR",
       shape = "YEAR",
       fill = "YEAR") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14,
                                      base_family = "sans") +

  theme(legend.position = "right") +
  
  geom_text(data = Yearlabel, aes(label = z),
            nudge_x = case_when(
              Yearlabel$z == "FY 2012/2013" ~ 50000,
              Yearlabel$z == "FY 2013/2014" ~ 14000,
              Yearlabel$z == "FY 2014/2015" ~ 14000,
              Yearlabel$z == "FY 2015/2016" ~ -10000,
              TRUE ~ 0
            ),            
            nudge_y = case_when(
              Yearlabel$z == "FY 2012/2013" ~ -60000,
              Yearlabel$z == "FY 2013/2014" ~ 10000,
              Yearlabel$z == "FY 2014/2015" ~ -100000,
              Yearlabel$z == "FY 2015/2016" ~ 50000,
              TRUE ~ 0
            ), # 根据label中的z值判断，不同的点，下移值不同，注意这里的值是目标图的尺寸
            size = 4,
            fontface = "bold",
            show.legend = FALSE)

  ggMarginal(.plot, type="histogram", fill = "goldenrod1", color = "olivedrab")
  rm(.df, .plot)
#------------------------ Multi-Factor KPI ------------------------#
# 3.1 Occupancy box & violin plot
  require("ggplot2")
.df <- data.frame(y = data$OCCUP,
                  z = data$SIZE,
                  s = data$REMOTE)
  # Stratum by SIZE, facet by REMOTE
.plot <- ggplot(data = .df, aes(x = z, y = y, fill = z)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_violin(width=1) +
  geom_boxplot(width=0.25)+
  facet_wrap(~ s) +
  xlab("Size") +
  ylab("Occupancy rate") +
  labs(fill = "Size") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14,
                                      base_family = "sans") +
  theme(panel.spacing = unit(0.3, "lines"))
  print(.plot)
  rm(.df, .plot)

# 3.2 Turnover box & violin plot
  require("ggplot2")
.df <- data.frame(y = data$TURNOVER,
                  z = data$SIZE,
                  s = data$REMOTE)
  # Stratum by SIZE, facet by REMOTE
.plot <- ggplot(data = .df, aes(x = z, y = y, fill = z)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_violin(width=1) +
  geom_boxplot(width=0.25)+
  facet_wrap(~ s) +
  xlab("Size") +
  ylab("Turnover rate") +
  labs(fill = "Size") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14,
                                      base_family = "sans") +
  theme(panel.spacing = unit(0.3, "lines"))
  print(.plot)
  rm(.df, .plot)

# 4.1 Occupancy kernel density
  require("ggplot2")
.df <- na.omit(data.frame(x = data$OCCUP))
.nbins <- pretty(range(.df$x), n = nclass.FD(.df$x), min.n = 1)
.plot <- ggplot(data = .df, aes(x = x, y = ..density..)) +
  # Epanechnikov kernel and CV bandwidth
  geom_density(
    kernel = "epanechnikov",
    bw = "ucv",
    alpha = 0.5,
    aes(color = SIZE, fill = SIZE)
  ) +
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab("Occupancy rate") +
  ylab("Density") +
  labs(colour = "Size",
       shape = "Size",
       fill = "Size") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14,
                                      base_family = "sans")
  print(.plot)
  rm(.df, .nbins, .plot)

# 4.2 Turnover kernel density
  require("ggplot2")
.df <- na.omit(data.frame(x = data$TURNOVER))
.nbins <- pretty(range(.df$x), n = nclass.FD(.df$x), min.n = 1)
.plot <- ggplot(data = .df, aes(x = x, y = ..density..)) +
  # Epanechnikov kernel and CV bandwidth
  geom_density(
    kernel = "epanechnikov",
    bw = "ucv",
    alpha = 0.5,
    aes(color = SIZE, fill = SIZE)
  ) +
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab("Turnover rate") +
  ylab("Density") +
  labs(colour = "Size",
       shape = "Size",
       fill = "Size") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14,
                                      base_family = "sans")
  print(.plot)
  rm(.df, .nbins, .plot)

# 4.3 In/outpatient ratio kernel density
  require("ggplot2")
.df <- na.omit(data.frame(x = data$INOUT))
.nbins <- pretty(range(.df$x), n = nclass.FD(.df$x), min.n = 1)
.plot <- ggplot(data = .df, aes(x = x, y = ..density..)) +
  # Epanechnikov kernel and CV bandwidth
  geom_density(
    kernel = "epanechnikov",
    bw = "ucv",
    alpha = 0.5,
    aes(color = SIZE, fill = SIZE)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab("Inpatient-outpatient ratio") +
  ylab("Density") +
  labs(colour = "Size",
       shape = "Size",
       fill = "Size") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14,
                                      base_family = "sans")
  print(.plot)
  rm(.df, .nbins, .plot)

# 5.1 Scatter plot of admissions to ave.LOS
  require("ggplot2")
  require("ggExtra")
.df <- data.frame(x = data$WEPS,
                  y = data$AVELOS,
                  z = data$SIZE)

# 创建数据点用于标签
Sizelabel <- .df %>%
  group_by(z) %>%
  slice(which.max(x))  # 选择每个分组的最大值所在的行

.plot <-
  ggplot(data = .df, aes(
    x = x,
    y = y,
    colour = z,
    shape = z)) +
  geom_point(size = 2.5) +
  # Non-parametric regression
  stat_smooth(aes(fill = z), method = "loess", alpha = 0.1, size = 1) +
  xlab("Case-mix adjusted admissions") +
  ylab("Average length of stay") +
  labs(colour = "Size",
       shape = "Size",
       fill = "Size") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14, base_family = "sans") +
  theme(legend.position = "right") +

# 新增label并控制位置
  geom_text(data = Sizelabel, aes(label = z),
          nudge_x = case_when(
            Sizelabel$z == "LARGE" ~ -2000,
            Sizelabel$z == "SMALL" ~ 0,
            Sizelabel$z == "VSMALL" ~ 11000,
            TRUE ~ 0
          ),            
          nudge_y = case_when(
            Sizelabel$z == "LARGE" ~ 2,
            Sizelabel$z == "SMALL" ~ 1.5,
            Sizelabel$z == "VSMALL" ~ 3.5,
            TRUE ~ 0
          ), # 根据label中的z值判断，不同的点，下移值不同，注意这里的值是目标图的尺寸
          size = 4,
          fontface = "bold",
          show.legend = FALSE)

  ggMarginal(.plot, type="histogram", fill = "goldenrod1", color = "olivedrab")
  rm(.df, .plot)

# 5.2 Scatter plot of admissions to log(ave.LOS)
  require("ggplot2")
  require("ggExtra")
.df <- data.frame(x = data$lWEPS,
                  y = data$AVELOS,
                  z = data$SIZE)

# 创建数据点用于标签
Sizelabel <- .df %>%
  group_by(z) %>%
  slice(which.max(x))  # 选择每个分组的最大值所在的行

.plot <-
  ggplot(data = .df, aes(
    x = x,
    y = y,
    colour = z,
    shape = z)) +
  geom_point(size = 2.5) +
  # Non-parametric regression
  stat_smooth(aes(fill = z), method = "loess", alpha = 0.1, size = 1) +
  xlab("Case-mix adjusted admissions in logarithm") +
  ylab("Average length of stay") +
  labs(colour = "Size",
       shape = "Size",
       fill = "Size") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14, base_family = "sans") +
  theme(legend.position = "right") +
  
# 新增label并控制位置
geom_text(data = Sizelabel, aes(label = z),
          nudge_x = case_when(
            Sizelabel$z == "LARGE" ~ -0.2,
            Sizelabel$z == "SMALL" ~ 0,
            Sizelabel$z == "VSMALL" ~ 0,
            TRUE ~ 0
          ),            
          nudge_y = case_when(
            Sizelabel$z == "LARGE" ~ 2.5,
            Sizelabel$z == "SMALL" ~ 2.5,
            Sizelabel$z == "VSMALL" ~ 5,
            TRUE ~ 0
          ), # 根据label中的z值判断，不同的点，下移值不同，注意这里的值是目标图的尺寸
          size = 4,
          fontface = "bold",
          show.legend = FALSE)


  ggMarginal(.plot, type="histogram", fill = "goldenrod1", color = "olivedrab")
  rm(.df, .plot)