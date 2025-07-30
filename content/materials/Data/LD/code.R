# Libraries
require(data.table)
require(ggplot2)
require(ggpubr)
require(usmap)
require(mblm)

# Custom Functions
sen <- function(..., weights = NULL) {
    mblm::mblm(...)
}

# Data read-in
legionell_cdcdat = fread(file = "./data/Qp4/legionnaire_disease/legionell_cdcdat.csv")
legionell_sparcs = fread("./data/Qp4/legionnaire_disease/legionell_sparcs.csv")
appndctis_sparcs = fread("./data/Qp4/legionnaire_disease/appndctis_sparcs.csv")
pneumonia_sparcs = fread("./data/Qp4/legionnaire_disease/pneumonia_sparcs.csv")
ischaemic_sparcs = fread("./data/Qp4/legionnaire_disease/ischaemic_sparcs.csv")
legionell_mean_cdcdat = fread("./data/Qp4/legionnaire_disease/legionell_mean_cdcdat.csv")
legionell_mean_sparcs = fread("./data/Qp4/legionnaire_disease/legionell_mean_sparcs.csv")
appndctis_mean_sparcs = fread("./data/Qp4/legionnaire_disease/appndctis_mean_sparcs.csv")
pneumonia_mean_sparcs = fread("./data/Qp4/legionnaire_disease/pneumonia_mean_sparcs.csv")
ischaemic_mean_sparcs = fread("./data/Qp4/legionnaire_disease/ischaemic_mean_sparcs.csv")
randomizedso2 = fread("./data/Qp4/legionnaire_disease/randomizedso2.csv")
randomizedso2_mean = fread("./data/Qp4/legionnaire_disease/randomizedso2_mean.csv")
ventile_CTdist_mean = fread("./data/Qp4/legionnaire_disease/ventile_CTdist_mean.csv")
ventile_CTdist_near = fread("./data/Qp4/legionnaire_disease/ventile_CTdist_near.csv")
legionell_CTloc = fread("./data/Qp4/legionnaire_disease/legionell_CTloc.csv")
ldtrend = fread("./data/Qp4/legionnaire_disease/ldtrend.csv")
ldstate = fread("./data/Qp4/legionnaire_disease/ldstate.csv")
figs3 = readRDS("./data/Qp4/legionnaire_disease/figs3.RDS")
ldctsensitivity_near = fread("./data/Qp4/legionnaire_disease/ldctsensitivity_near.csv")
ldctsensitivity_mean = fread("./data/Qp4/legionnaire_disease/ldctsensitivity_mean.csv")
towersclean = fread("./data/Qp4/legionnaire_disease/towersclean.csv")
legotowers = fread("./data/Qp4/legionnaire_disease/legotowers.csv")

# FIG 1
fig1_ab = ggarrange(
    plot_usmap(
        data = ldstate,
        values = "cases.1519",
        color = "black",
        size = 0.1
    ) +
        scale_fill_viridis_c(
            name = "Cases/year",
            label = scales::comma,
            guide = 'colorbar',
            option = "B",
            limits = c(0, 1020)
        ) +
        theme(
            legend.position = "bottom",
            legend.key.width = unit(1.5, "lines"),
            legend.key.height = unit(0.25, "lines")
        ) +
        ggtitle("A 2015–2019 LD cases per year"),
    
    plot_usmap(
        data = ldstate,
        values = "cases.ratio",
        color = "black",
        size = 0.1
    ) +
        scale_fill_viridis_c(
            name = "Ratio",
            label = scales::comma,
            guide = 'colorbar',
            option = "B",
            trans = "log10"
        ) +
        theme(
            legend.position = "bottom",
            legend.key.width = unit(1.5, "lines"),
            legend.key.height = unit(0.25, "lines")
        ) +
        ggtitle("B 2015–2019:1992–2001 LD ratio"),
    
    ncol = 2
)
fig1_c = ggplot(ldtrend,
                aes(
                    x = year,
                    y = value,
                    col = .id,
                    shape = .id,
                    linetype = variable
                )) +
    geom_point() +
    geom_line() +
    scale_y_continuous(
        name = 'LD cases',
        breaks = seq(0, 11000, 500),
        labels = c(
            0,
            rep("", 1),
            1000,
            rep("", 1),
            2000,
            rep("", 1),
            3000,
            rep("", 1),
            4000,
            rep("", 1),
            5000,
            rep("", 1),
            6000,
            rep("", 1),
            7000,
            rep("", 1),
            8000,
            rep("", 1),
            9000,
            rep("", 1),
            10000,
            rep("", 1),
            11000
        ),
        limits = c(0, 11000),
        sec.axis = sec_axis(
            ~ . / (11000 / 15),
            name = "Ratio of LD to LD1992-2001",
            breaks = seq(0, 15, 1),
            labels = c(
                0,
                rep("", 1),
                2,
                rep("", 1),
                4,
                rep("", 1),
                6,
                rep("", 1),
                8,
                rep("", 1),
                10,
                rep("", 1),
                12,
                rep("", 1),
                14,
                rep("", 1)
            )
        ),
        expand = c(0, 0)
    ) +
    scale_x_continuous(
        name = 'Year',
        breaks = seq(1990, 2020, 1),
        labels = c(
            1990,
            rep("", 4),
            1995,
            rep("", 4),
            2000,
            rep("", 4),
            2005,
            rep("", 4),
            2010,
            rep("", 4),
            2015,
            rep("", 4),
            2020
        ),
        limits = c(1990, 2020),
        expand = c(0, 0)
    ) +
    scale_color_manual(values = c("#d7191c", "black", "#2c7bb6")) +
    scale_shape_manual(values = c(15, 8, 19)) +
    theme_bw() +
    theme(
        panel.grid = element_blank(),
        strip.text.x = element_blank(),
        legend.position = c(0.2, 0.7)
    ) +
    ggtitle("C Trend of LD in NYS, NEUS, and US")
ggarrange(fig1_ab, fig1_c, nrow = 2, heights = c(1, 2.5))

#' FIG 2 +++
#' Figure generated using Xmgr (https://plasma-gate.weizmann.ac.il/Xmgr/)
#' and the dataset fig2.txt

#' FIG 3B +++
#' Figure generated using Xmgr (https://plasma-gate.weizmann.ac.il/Xmgr/)
#' and the dataset fig3b.txt

# FIG 3C
ggplot(
    legionell_cdcdat,
    aes(
        xmin = low,
        xmax = high,
        ymin = value - 0.5,
        ymax = value + 0.5,
        fill = count
    )
) +
    geom_rect() +
    geom_smooth(
        method = "gam",
        data = legionell_mean_cdcdat,
        aes(x = mid, y = value * 8),
        col = "red",
        fill = "red",
        size = 1
    ) +
    geom_point(
        data = legionell_mean_cdcdat,
        aes(x = mid, y = value * 8),
        col = "red",
        fill = NA,
        size = 1.5,
        pch = 21
    ) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 8, name = "Average cases per week")) +
    scale_x_log10() +
    scale_fill_viridis_c("Weeks", trans = "log10", option = "H") +
    annotation_logticks(sides = "b") +
    labs(x = "SO2 (ppbv)", y = "Case Count") +
    theme_bw() + theme(
        panel.grid = element_blank(),
        legend.position = c(0.87, 0.65),
        legend.key.height = unit(1.5, "lines"),
        legend.key.width = unit(0.5, "lines"),
        axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right =  element_text(color = "red"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        text = element_text(family = "Times New Roman")
    ) +
    coord_cartesian(xlim = c(0.067, 37), ylim = c(0.5, 20))


# FIG 4A-C
NY_base <-
    ggplot(data = legionell_CTloc,
           mapping = aes(x = long, y = lat, group = subregion)) +
    coord_fixed(1.3) +
    geom_polygon(color = "black",
                 fill = "gray",
                 size = 0.1)

ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
    legend.position = 'top',
    legend.justification = 'left',
    legend.direction = 'horizontal',
    legend.key.width = unit(1.5, "lines"),
    legend.key.height = unit(0.25, "lines")
)

NY_basex = NY_base +
    geom_polygon(aes(fill = Cases / 5), color = "black", size = 0.1) +
    theme_bw() +
    ditch_the_axes +
    scale_fill_viridis_c("", option = "B", trans = "log10")

NY_basez = NY_base +
    geom_polygon(fill = "white",
                 col = "black",
                 size = 0.1) +
    geom_bin2d(
        data = towersclean,
        aes(lon, lat),
        inherit.aes = F,
        binwidth = 0.05
    ) +
    theme_bw() +
    ditch_the_axes +
    scale_fill_viridis_c("", option = "D", trans = "log10")

insetplo = ggplot(legotowers, aes(x = n_tower, y = Cases / 5)) +
    geom_point() +
    geom_smooth(method = sen,
                se = F,
                col = "blue") +
    scale_x_log10() +
    scale_y_log10() +
    annotation_logticks(sides = "trbl") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(x = "Cooling towers", y = "LD cases per year")

ggarrange(
    NY_basex + ggtitle("A LD cases per year"),
    NY_basez + ggtitle("B Cooling towers (CT)"),
    insetplo + coord_equal() + ggtitle("C LD cases per year vs. CT"),
    widths = c(1, 1, 1.2),
    ncol = 3
)

# FIG 4D
fig4d_inset = ggplot(
    ventile_CTdist_near[.id == "2000-2004"],
    aes(
        x = (centile_appdcts_lab + low) / 2,
        y = n,
        xmin = low,
        xmax = centile_appdcts_lab,
        ymin = n - 5,
        ymax = n + 5,
        fill = .id,
        col = .id
    )
) +
    geom_rect(alpha = 0.5) +
    geom_point(size = 0.2) +
    #stat_smooth(method="gam", se=TRUE)+
    stat_smooth(
        method = "lm",
        se = TRUE,
        formula = y ~ poly(x, 3, raw = TRUE)
    ) +
    #geom_vline(data=elbowframe,aes(xintercept=elbow), linetype = "dashed")+
    scale_x_log10() +
    coord_cartesian(xlim = c(50, 25000)) +
    theme_bw() +
    theme(
        text = element_text(family = "Times New Roman"),
        panel.grid = element_blank(),
        legend.position = "none"
    ) +
    scale_fill_manual(values = c("#5e3c99", "#e66101")) +
    scale_color_manual(values = c("#5e3c99", "#e66101")) +
    annotation_logticks(sides = "b") +
    labs(x = "Distance (m)" , y = "n")

fig4d_main  = ggplot(
    ventile_CTdist_near,
    aes(
        x = (centile_appdcts_lab + low) / 2,
        y = n,
        xmin = low,
        xmax = centile_appdcts_lab,
        ymin = n - 5,
        ymax = n + 5,
        fill = .id,
        col = .id
    )
) +
    geom_rect(alpha = 0.5) +
    geom_point(size = 0.2) +
    # stat_smooth(method="gam", se=TRUE)+
    stat_smooth(
        method = "lm",
        se = TRUE,
        formula = y ~ poly(x, 3, raw = TRUE)
    ) +
    #geom_vline(data=elbowframe,aes(xintercept=elbow), linetype = "dashed")+
    scale_x_log10() +
    coord_cartesian(xlim = c(50, 20000)) +
    theme_bw() +
    facet_grid(. ~ .id) +
    theme(
        text = element_text(family = "Times New Roman"),
        panel.grid = element_blank(),
        legend.position = "none"
    ) +
    scale_fill_manual(values = c("#5e3c99", "#e66101")) +
    scale_color_manual(values = c("#5e3c99", "#e66101")) +
    annotation_logticks(sides = "b")


# FIG 4E
fig4e_inset = ggplot(
    ventile_CTdist_mean[.id == "2000-2004"],
    aes(
        x = (ventile_appdcts_lab + low) / 2,
        y = n,
        xmin = low,
        xmax = ventile_appdcts_lab,
        ymin = n - 5,
        ymax = n + 5,
        fill = .id,
        col = .id
    )
) +
    geom_rect(alpha = 0.5) +
    geom_point(size = 0.2) +
    #stat_smooth(method="gam", se=TRUE)+
    stat_smooth(
        method = "lm",
        se = TRUE,
        formula = y ~ poly(x, 3, raw = TRUE)
    ) +
    #geom_vline(data=elbowframe,aes(xintercept=elbow), linetype = "dashed")+
    scale_x_log10() +
    coord_cartesian(xlim = c(1100, 9100)) +
    theme_bw() +
    theme(
        text = element_text(family = "Times New Roman"),
        panel.grid = element_blank(),
        legend.position = "none"
    ) +
    scale_fill_manual(values = c("#5e3c99", "#e66101")) +
    scale_color_manual(values = c("#5e3c99", "#e66101")) +
    annotation_logticks(sides = "b") +
    labs(x = "Distance (m)" , y = "n")

fig4e_main = ggplot(
    ventile_CTdist_mean,
    aes(
        x = (ventile_appdcts_lab + low) / 2,
        y = n,
        xmin = low,
        xmax = ventile_appdcts_lab,
        ymin = n - 5,
        ymax = n + 5,
        fill = .id,
        col = .id
    )
) +
    geom_rect(alpha = 0.5) +
    geom_point(size = 0.2) +
    #stat_smooth(method="gam", se=TRUE)+
    stat_smooth(
        method = "lm",
        se = TRUE,
        formula = y ~ poly(x, 3, raw = TRUE)
    ) +
    #geom_vline(data=elbowframe,aes(xintercept=elbow), linetype = "dashed")+
    scale_x_log10() +
    coord_cartesian(xlim = c(1350, 9100)) +
    theme_bw() +
    facet_grid(. ~ .id) +
    theme(
        text = element_text(family = "Times New Roman"),
        panel.grid = element_blank(),
        legend.position = "none"
    ) +
    scale_fill_manual(values = c("#5e3c99", "#e66101")) +
    scale_color_manual(values = c("#5e3c99", "#e66101")) +
    annotation_logticks(sides = "b")

# SUPPLEMENTARY FIGURES
# FIG S1
ggplot(randomizedso2,
       aes(
           xmin = low,
           xmax = high,
           ymin = value - 0.5,
           ymax = value + 0.5,
           fill = count
       )) +
    geom_rect() +
    geom_smooth(
        method = "gam",
        data = randomizedso2_mean,
        aes(x = mid, y = value * 8),
        col = "red",
        fill = "red",
        size = 1
    ) +
    geom_point(
        data = randomizedso2_mean,
        aes(x = mid, y = value * 8),
        col = "red",
        fill = NA,
        size = 1.5,
        pch = 21
    ) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 8, name = "Average LD cases per week")) +
    scale_x_log10() +
    scale_fill_viridis_c("Weeks", trans = "log10", option = "H") +
    facet_grid(. ~ variable) + #variable
    annotation_logticks(sides = "b") +
    labs(x = "SO2 (ppbv)", y = "LD Case Count") +
    theme_bw() + theme(
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(0.5, "lines"),
        legend.key.width = unit(3, "lines"),
        axis.line.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.right =  element_text(color = "red"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        text = element_text(family = "Times New Roman")
    ) +
    coord_cartesian(xlim = c(0.067, 37), ylim = c(0.5, 20))

# Fig S2
ggarrange(
    ggplot(
        legionell_sparcs,
        aes(
            xmin = low,
            xmax = high,
            ymin = value - 0.5,
            ymax = value + 0.5,
            fill = count
        )
    ) +
        geom_rect() +
        geom_smooth(
            method = "gam",
            data = legionell_mean_sparcs,
            aes(x = mid, y = value * 8),
            col = "red",
            fill = "red",
            size = 1
        ) +
        geom_point(
            data = legionell_mean_sparcs,
            aes(x = mid, y = value * 8),
            col = "red",
            fill = NA,
            size = 1.5,
            pch = 21
        ) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 8, name = "Average cases per week")) +
        scale_x_log10() +
        scale_fill_viridis_c("Weeks", trans = "log10", option = "H") +
        annotation_logticks(sides = "b") +
        labs(x = "SO2 (ppbv)", y = "LD Case Count", title = "Legionnaires' Disease") +
        theme_bw() + theme(
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.key.height = unit(0.5, "lines"),
            legend.key.width = unit(1.5, "lines"),
            axis.line.y.right = element_line(color = "red"),
            axis.text.y.right = element_text(color = "red"),
            axis.ticks.y.right = element_line(color = "red"),
            axis.title.y.right =  element_text(color = "red"),
            legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"),
            text = element_text(family = "Times New Roman")
        ) +
        coord_cartesian(xlim = c(0.067, 37), ylim = c(0.5, 20)),
    ggplot(
        appndctis_sparcs,
        aes(
            xmin = low,
            xmax = high,
            ymin = value - 0.5,
            ymax = value + 0.5,
            fill = count
        )
    ) +
        geom_rect() +
        geom_smooth(
            method = "gam",
            data = appndctis_mean_sparcs,
            aes(x = mid, y = value * 2),
            col = "red",
            fill = "red",
            size = 1
        ) +
        geom_point(
            data = appndctis_mean_sparcs,
            aes(x = mid, y = value * 2),
            col = "red",
            fill = NA,
            size = 1.5,
            pch = 21
        ) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 2, name = "Average cases per week")) +
        scale_x_log10() +
        scale_fill_viridis_c("Weeks", trans = "log10", option = "H") +
        annotation_logticks(sides = "b") +
        labs(x = "SO2 (ppbv)", y = "Case Count", title = "Acute appendicitis") +
        theme_bw() + theme(
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.key.height = unit(0.5, "lines"),
            legend.key.width = unit(1.5, "lines"),
            axis.line.y.right = element_line(color = "red"),
            axis.text.y.right = element_text(color = "red"),
            axis.ticks.y.right = element_line(color = "red"),
            axis.title.y.right =  element_text(color = "red"),
            legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"),
            text = element_text(family = "Times New Roman")
        ) +
        coord_cartesian(xlim = c(0.067, 37), ylim = c(2, 50)),
    ggplot(
        pneumonia_sparcs,
        aes(
            xmin = low,
            xmax = high,
            ymin = value - 0.5,
            ymax = value + 0.5,
            fill = count
        )
    ) +
        geom_rect() +
        geom_smooth(
            method = "gam",
            data = pneumonia_mean_sparcs,
            aes(x = mid, y = value * 4),
            col = "red",
            fill = "red",
            size = 1
        ) +
        geom_point(
            data = pneumonia_mean_sparcs,
            aes(x = mid, y = value * 4),
            col = "red",
            fill = NA,
            size = 1.5,
            pch = 21
        ) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 4, name = "Average cases per week")) +
        scale_x_log10() +
        scale_fill_viridis_c("Weeks", trans = "log10", option = "H") +
        annotation_logticks(sides = "b") +
        labs(x = "SO2 (ppbv)", y = "Case Count", title = "Streptococcal pneumonia") +
        theme_bw() + theme(
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.key.height = unit(0.5, "lines"),
            legend.key.width = unit(1.5, "lines"),
            axis.line.y.right = element_line(color = "red"),
            axis.text.y.right = element_text(color = "red"),
            axis.ticks.y.right = element_line(color = "red"),
            axis.title.y.right =  element_text(color = "red"),
            legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"),
            text = element_text(family = "Times New Roman")
        ) +
        coord_cartesian(xlim = c(0.067, 37), ylim = c(0.5, 16)),
    ggplot(
        ischaemic_sparcs,
        aes(
            xmin = low,
            xmax = high,
            ymin = value - 0.5,
            ymax = value + 0.5,
            fill = count
        )
    ) +
        geom_rect() +
        geom_smooth(
            method = "gam",
            data = ischaemic_mean_sparcs,
            aes(x = mid, y = value * 1.75),
            col = "red",
            fill = "red",
            size = 1
        ) +
        geom_point(
            data = ischaemic_mean_sparcs,
            aes(x = mid, y = value * 1.75),
            col = "red",
            fill = NA,
            size = 1.5,
            pch = 21
        ) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1.75, name = "Average cases per week")) +
        scale_x_log10() +
        scale_fill_viridis_c("Weeks", trans = "log10", option = "H") +
        annotation_logticks(sides = "b") +
        labs(x = "SO2 (ppbv)", y = "Case Count", title = "Chronic Ischemic Heart Disease") +
        theme_bw() + theme(
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.key.height = unit(0.5, "lines"),
            legend.key.width = unit(1.5, "lines"),
            axis.line.y.right = element_line(color = "red"),
            axis.text.y.right = element_text(color = "red"),
            axis.ticks.y.right = element_line(color = "red"),
            axis.title.y.right =  element_text(color = "red"),
            legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"),
            text = element_text(family = "Times New Roman")
        ) +
        coord_cartesian(xlim = c(0.067, 37), ylim = c(2, 200)),
    labels = c("a", "b", "c", "d"),
    ncol = 2,
    nrow = 2,
    align = "hv"
)

#' FIG S3
#' Only the processed density profiles are provided here due to data restrictions
#' Please use ggplot2-v3.4.1
figs3$plot

# FIG S4
ggplot(ldctsensitivity_near, aes(x = centile_lab, y = n, col = .id)) +
    geom_point() +
    
    stat_smooth(
        method = "lm",
        se = TRUE,
        formula = y ~ poly(x, 3, raw = TRUE)
    ) +
    scale_x_log10() + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(xlabel ~
                                                                                        .id, scales = "free") +
    theme(
        text = element_text(family = "Times New Roman"),
        legend.position = "none",
        strip.background = element_blank()
    ) + annotation_logticks(sides = "b") +
    labs(y = "Number of Hospitalizations", x = "Distance (meters)")

# FIG S5
ggplot(ldctsensitivity_mean, aes(x = ventile_lab, y = n, col = .id)) +
    geom_point() +
    stat_smooth(
        method = "lm",
        se = TRUE,
        formula = y ~ poly(x, 3, raw = TRUE)
    ) +
    scale_x_log10() + theme_bw() + theme(panel.grid = element_blank()) + facet_grid(xlabel ~
                                                                                        .id, scales = "free") +
    theme(
        text = element_text(family = "Times New Roman"),
        legend.position = "none",
        strip.background = element_blank()
    ) + annotation_logticks(sides = "b") +
    labs(y = "Number of Hospitalizations", x = "Distance (meters)")
