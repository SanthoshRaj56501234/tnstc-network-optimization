# ============================================================
# TNSTC VOLVO BUS - NETWORK OPTIMIZATION SHINY APP
# Revenue Management Project
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(plotly)
library(DT)
library(tidyr)

# ---------------------------------------------------------------
# LOAD DATA (from CSVs if available, else generate inline)
# ---------------------------------------------------------------
load_or_generate <- function() {
  if (all(file.exists(c("tnstc_network_schedules.csv",
                         "tnstc_network_bookings.csv",
                         "tnstc_network_route_perf.csv",
                         "tnstc_network_nodes.csv",
                         "tnstc_network_transfers.csv")))) {
    schedule_df  <- read.csv("tnstc_network_schedules.csv",  stringsAsFactors = FALSE)
    booking_df   <- read.csv("tnstc_network_bookings.csv",   stringsAsFactors = FALSE)
    route_perf   <- read.csv("tnstc_network_route_perf.csv", stringsAsFactors = FALSE)
    node_metrics <- read.csv("tnstc_network_nodes.csv",      stringsAsFactors = FALSE)
    transfer_df  <- read.csv("tnstc_network_transfers.csv",  stringsAsFactors = FALSE)
    schedule_df$travel_date  <- as.Date(schedule_df$travel_date)
    booking_df$travel_date   <- as.Date(booking_df$travel_date)
    transfer_df$travel_date  <- as.Date(transfer_df$travel_date)
  } else {
    set.seed(123)
    edges <- data.frame(
      origin        = c("BENGALURU","CHENNAI","CHENNAI","CHENNAI","CHENNAI",
                        "CHENNAI","CHENNAI","CHENNAI","CHENNAI","CHENNAI",
                        "COIMBATORE","MADURAI","NAGERCOIL","NAGERCOIL","NAGERCOIL",
                        "THANJAVUR","TIRUCHENDUR","TIRUCHENDUR","TIRUNELVELI","TIRUNELVELI","TIRUPPUR"),
      destination   = c("CHENNAI","COIMBATORE","MADURAI","NAGERCOIL","SALEM",
                        "THANJAVUR","TIRUCHENDUR","TIRUNELVELI","TIRUPPUR","TRICHY",
                        "SALEM","TRICHY","MADURAI","TIRUNELVELI","TRICHY",
                        "TRICHY","MADURAI","TRICHY","MADURAI","TRICHY","SALEM"),
      base_fare_inr = c(680,825,740,1145,535,545,1050,1020,750,530,
                        295,220,420,145,650,120,320,535,280,510,210),
      distance_km   = c(346,497,463,680,337,347,630,631,480,336,
                        165,135,241,84,395,77,195,327,175,315,132),
      stringsAsFactors = FALSE
    )
    edges$travel_time_hrs <- round(edges$distance_km / 55, 2)

    nodes_raw <- data.frame(
      station   = c("BENGALURU","CHENNAI","COIMBATORE","MADURAI","NAGERCOIL",
                    "SALEM","THANJAVUR","TIRUCHENDUR","TIRUNELVELI","TIRUPPUR","TRICHY"),
      node_type = c("Hub","Hub","Hub","Hub","Terminal","Intermediate",
                    "Intermediate","Terminal","Hub","Intermediate","Hub"),
      lat = c(12.97,12.98,11.00,9.93,8.18,11.65,10.79,8.50,8.73,11.10,10.81),
      lon = c(77.59,80.25,76.96,78.12,77.43,78.16,79.14,78.12,77.70,77.34,78.69),
      stringsAsFactors = FALSE
    )

    bus_types <- data.frame(
      bus_type    = c("Volvo AC Sleeper","Volvo AC Semi-Sleeper","Volvo AC Seater"),
      total_seats = c(36, 40, 44),
      cost_per_km = c(28, 24, 20),
      fixed_cost  = c(3500, 3000, 2500),
      stringsAsFactors = FALSE
    )

    n_sc <- 500
    sd   <- as.Date("2024-09-01"); ed <- as.Date("2025-02-28")
    sdates <- sample(seq(sd,ed,by="day"),n_sc,replace=TRUE)
    sei  <- sample(1:nrow(edges),   n_sc,replace=TRUE)
    sbi  <- sample(1:nrow(bus_types),n_sc,replace=TRUE,prob=c(.4,.4,.2))

    get_s <- function(d){ m<-month(d); dplyr::case_when(
      m%in%c(4,5)~"Summer",m%in%c(6,7,8)~"Monsoon",m%in%c(10,11)~"Festive",
      m==12|m%in%c(1,2)~"Winter",TRUE~"Normal") }
    hols <- as.Date(c("2024-10-02","2024-10-12","2024-10-24","2024-11-01","2024-11-15",
                       "2024-12-25","2025-01-01","2025-01-14","2025-01-15","2025-01-26","2025-02-26"))

    schedule_df <- data.frame(
      schedule_id=paste0("SCH",sprintf("%04d",1:n_sc)),
      travel_date=sdates,season=get_s(sdates),is_holiday=sdates%in%hols,
      day_of_week=weekdays(sdates),is_weekend=wday(sdates)%in%c(1,7),
      origin=edges$origin[sei],destination=edges$destination[sei],
      distance_km=edges$distance_km[sei],travel_time_hrs=edges$travel_time_hrs[sei],
      base_fare_inr=edges$base_fare_inr[sei],bus_type=bus_types$bus_type[sbi],
      total_seats=bus_types$total_seats[sbi],cost_per_km=bus_types$cost_per_km[sbi],
      fixed_cost_inr=bus_types$fixed_cost[sbi],
      departure_hour=sample(c(5,6,7,8,10,14,17,18,19,20,21,22),n_sc,replace=TRUE,
                            prob=c(.04,.05,.06,.06,.08,.07,.10,.12,.12,.10,.10,.10)),
      stringsAsFactors=FALSE)
    db <- ifelse(schedule_df$season=="Festive",.88,ifelse(schedule_df$season=="Summer",.80,
          ifelse(schedule_df$season=="Monsoon",.55,ifelse(schedule_df$season=="Winter",.72,.65))))
    db <- pmin(db+ifelse(schedule_df$is_weekend,.08,0)+ifelse(schedule_df$is_holiday,.10,0),.98)
    schedule_df$passengers_booked <- round(schedule_df$total_seats*pmin(pmax(db+rnorm(n_sc,0,.07),.15),1))
    schedule_df$occupancy_rate    <- round(schedule_df$passengers_booked/schedule_df$total_seats,3)
    schedule_df$seats_available   <- schedule_df$total_seats-schedule_df$passengers_booked
    fm <- ifelse(schedule_df$occupancy_rate>=.90,runif(n_sc,1.30,1.50),
          ifelse(schedule_df$occupancy_rate>=.75,runif(n_sc,1.15,1.30),
          ifelse(schedule_df$occupancy_rate>=.50,runif(n_sc,1.00,1.15),runif(n_sc,.85,1.00))))
    schedule_df$fare_multiplier   <- round(fm,3)
    schedule_df$actual_fare_inr   <- round(schedule_df$base_fare_inr*fm/5)*5
    schedule_df$total_revenue_inr <- schedule_df$actual_fare_inr*schedule_df$passengers_booked
    schedule_df$variable_cost_inr <- schedule_df$cost_per_km*schedule_df$distance_km
    schedule_df$total_cost_inr    <- schedule_df$fixed_cost_inr+schedule_df$variable_cost_inr
    schedule_df$profit_inr        <- schedule_df$total_revenue_inr-schedule_df$total_cost_inr
    schedule_df$profit_margin_pct <- round(schedule_df$profit_inr/schedule_df$total_revenue_inr*100,2)
    schedule_df$revenue_per_km    <- round(schedule_df$total_revenue_inr/schedule_df$distance_km,2)
    schedule_df$load_factor       <- schedule_df$occupancy_rate
    dp  <- ifelse(schedule_df$season=="Monsoon",.35,ifelse(schedule_df$is_holiday,.25,.12))
    schedule_df$delay_occurred <- runif(n_sc)<dp
    schedule_df$delay_minutes  <- ifelse(schedule_df$delay_occurred,round(runif(n_sc,10,90)),0)
    schedule_df$on_time        <- !schedule_df$delay_occurred

    bp  <- c(.05,.07,.07,.06,.06,.05,.05,.05,.04,.04,.04,.04,.03,.03,.03,
             .03,.03,.03,.03,.02,.02,.02,.02,.02,.02,.02,.02,.02,.02,.02,.02)
    bp  <- bp/sum(bp)
    nb  <- 4000; bs <- sample(1:n_sc,nb,replace=TRUE)
    booking_df <- data.frame(
      booking_id=paste0("BK",sprintf("%05d",1:nb)),
      schedule_id=schedule_df$schedule_id[bs],origin=schedule_df$origin[bs],
      destination=schedule_df$destination[bs],travel_date=schedule_df$travel_date[bs],
      season=schedule_df$season[bs],is_holiday=schedule_df$is_holiday[bs],
      bus_type=schedule_df$bus_type[bs],distance_km=schedule_df$distance_km[bs],
      days_in_advance=sample(0:30,nb,replace=TRUE,prob=bp),
      booking_channel=sample(c("Online","Counter","Phone","Agent"),nb,replace=TRUE,prob=c(.55,.25,.10,.10)),
      seat_class=sample(c("Sleeper","Semi-Sleeper","Seater"),nb,replace=TRUE,prob=c(.45,.38,.17)),
      passenger_type=sample(c("Adult","Senior","Student","Child"),nb,replace=TRUE,prob=c(.70,.12,.12,.06)),
      stringsAsFactors=FALSE)
    am <- ifelse(booking_df$days_in_advance<=1,runif(nb,1.25,1.50),
          ifelse(booking_df$days_in_advance<=3,runif(nb,1.10,1.25),
          ifelse(booking_df$days_in_advance<=7,runif(nb,1.00,1.10),
          ifelse(booking_df$days_in_advance<=14,runif(nb,.90,1.00),runif(nb,.80,.92)))))
    pd  <- ifelse(booking_df$passenger_type=="Senior",.90,
           ifelse(booking_df$passenger_type=="Student",.85,
           ifelse(booking_df$passenger_type=="Child",.50,1.00)))
    booking_df$fare_paid_inr   <- round(schedule_df$base_fare_inr[bs]*am*pd/5)*5
    booking_df$cancellation    <- runif(nb)<.06
    booking_df$refund_inr      <- ifelse(booking_df$cancellation,round(booking_df$fare_paid_inr*.75),0)
    booking_df$net_revenue_inr <- ifelse(booking_df$cancellation,
                                         booking_df$fare_paid_inr-booking_df$refund_inr,booking_df$fare_paid_inr)

    route_perf <- schedule_df %>%
      group_by(origin,destination) %>%
      summarise(distance_km=first(distance_km),travel_time_hrs=first(travel_time_hrs),
                base_fare_inr=first(base_fare_inr),total_schedules=n(),
                avg_occupancy_rate=round(mean(occupancy_rate),3),
                avg_fare_inr=round(mean(actual_fare_inr)),
                total_passengers=sum(passengers_booked),
                total_revenue_inr=sum(total_revenue_inr),total_cost_inr=sum(total_cost_inr),
                total_profit_inr=sum(profit_inr),avg_profit_margin=round(mean(profit_margin_pct),2),
                avg_revenue_per_km=round(mean(revenue_per_km),2),
                on_time_rate=round(mean(on_time),3),avg_delay_min=round(mean(delay_minutes),1),
                .groups="drop") %>%
      mutate(route_label=paste(origin,"->",destination),
             profitability_class=ifelse(avg_profit_margin>=30,"High",
                                 ifelse(avg_profit_margin>=15,"Medium","Low")),
             demand_class=ifelse(avg_occupancy_rate>=.80,"High Demand",
                          ifelse(avg_occupancy_rate>=.60,"Moderate","Low Demand")))

    deps <- schedule_df %>% group_by(station=origin) %>%
      summarise(departing_trips=n(),departing_passengers=sum(passengers_booked),
                departing_revenue=sum(total_revenue_inr),.groups="drop")
    arrs <- schedule_df %>% group_by(station=destination) %>%
      summarise(arriving_trips=n(),arriving_passengers=sum(passengers_booked),.groups="drop")
    node_metrics <- nodes_raw %>% left_join(deps,by="station") %>% left_join(arrs,by="station") %>%
      mutate(across(where(is.numeric),~replace(.,is.na(.),0)),
             total_throughput=departing_passengers+arriving_passengers,
             connectivity_degree=sapply(station,function(s) sum(edges$origin==s|edges$destination==s)),
             avg_platform_util=round(pmin((departing_trips+arriving_trips)/60,1),3),
             hub_score=round((connectivity_degree/max(connectivity_degree))*100,1))

    tp <- data.frame(
      origin_node=c("BENGALURU","BENGALURU","COIMBATORE","COIMBATORE","SALEM",
                    "SALEM","THANJAVUR","THANJAVUR","TIRUCHENDUR","TIRUPPUR"),
      hub_node=c("CHENNAI","CHENNAI","CHENNAI","TRICHY","CHENNAI",
                 "TRICHY","TRICHY","MADURAI","MADURAI","TRICHY"),
      dest_node=c("MADURAI","TRICHY","MADURAI","MADURAI","MADURAI",
                  "MADURAI","NAGERCOIL","NAGERCOIL","TRICHY","MADURAI"),
      stringsAsFactors=FALSE)
    nt <- 800; ti <- sample(1:nrow(tp),nt,replace=TRUE)
    transfer_df <- data.frame(
      transfer_id=paste0("TF",sprintf("%04d",1:nt)),
      travel_date=sample(seq(sd,ed,by="day"),nt,replace=TRUE),
      origin=tp$origin_node[ti],transfer_hub=tp$hub_node[ti],
      final_destination=tp$dest_node[ti],stringsAsFactors=FALSE)
    transfer_df$season           <- get_s(transfer_df$travel_date)
    transfer_df$waiting_time_min <- round(pmax(rnorm(nt,45,20),10))
    transfer_df$connection_made  <- transfer_df$waiting_time_min<=90 & runif(nt)>.08
    transfer_df$leg1_fare_inr    <- sample(c(295,346,536,680,497),nt,replace=TRUE)
    transfer_df$leg2_fare_inr    <- sample(c(220,120,280,320,145),nt,replace=TRUE)
    transfer_df$total_fare_inr   <- transfer_df$leg1_fare_inr+transfer_df$leg2_fare_inr
    transfer_df$direct_fare_inr  <- round(transfer_df$total_fare_inr*runif(nt,.80,.95))
    transfer_df$fare_penalty_inr <- transfer_df$total_fare_inr-transfer_df$direct_fare_inr
    transfer_df$passenger_type   <- sample(c("Adult","Senior","Student","Child"),nt,replace=TRUE,
                                           prob=c(.70,.12,.12,.06))
  }
  list(schedule=schedule_df, booking=booking_df, route=route_perf,
       node=node_metrics, transfer=transfer_df)
}

dat          <- load_or_generate()
schedule_df  <- dat$schedule
booking_df   <- dat$booking
route_perf   <- dat$route
node_metrics <- dat$node
transfer_df  <- dat$transfer

edges_master <- data.frame(
  origin      = c("BENGALURU","CHENNAI KILAMBAKKAM KCBT","CHENNAI KILAMBAKKAM KCBT",
                  "CHENNAI KILAMBAKKAM KCBT","CHENNAI KILAMBAKKAM KCBT",
                  "CHENNAI KILAMBAKKAM KCBT","CHENNAI KILAMBAKKAM KCBT",
                  "CHENNAI KILAMBAKKAM KCBT","CHENNAI KILAMBAKKAM KCBT",
                  "CHENNAI KILAMBAKKAM KCBT","COIMBATORE","MADURAI",
                  "NAGERCOIL","NAGERCOIL","NAGERCOIL","THANJAVUR",
                  "TIRUCHENDUR","TIRUCHENDUR","TIRUNELVELI","TIRUNELVELI","TIRUPPUR"),
  destination = c("CHENNAI KILAMBAKKAM KCBT","COIMBATORE","MADURAI","NAGERCOIL","SALEM",
                  "THANJAVUR","TIRUCHENDUR","TIRUNELVELI","TIRUPPUR","TRICHY","SALEM",
                  "TRICHY","MADURAI","TIRUNELVELI","TRICHY","TRICHY","MADURAI","TRICHY",
                  "MADURAI","TRICHY","SALEM"),
  stringsAsFactors = FALSE
)

station_coords <- data.frame(
  station   = c("BENGALURU","CHENNAI","COIMBATORE","MADURAI","NAGERCOIL",
                "SALEM","THANJAVUR","TIRUCHENDUR","TIRUNELVELI","TIRUPPUR","TRICHY"),
  node_type = c("Hub","Hub","Hub","Hub","Terminal","Intermediate",
                "Intermediate","Terminal","Hub","Intermediate","Hub"),
  lat = c(12.97,12.98,11.00,9.93,8.18,11.65,10.79,8.50,8.73,11.10,10.81),
  lon = c(77.59,80.25,76.96,78.12,77.43,78.16,79.14,78.12,77.70,77.34,78.69),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------
# CSS
# ---------------------------------------------------------------
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@400;500;600;700;800&family=Source+Sans+3:wght@300;400;500;600&display=swap');

*,*::before,*::after{box-sizing:border-box;}
body,.content-wrapper,.wrapper{
  background-color:#F0F4F8!important;
  font-family:'Source Sans 3',sans-serif;color:#1A2332;}
.skin-blue .main-header .logo,
.skin-blue .main-header .navbar{background-color:#1B4F72!important;border-bottom:3px solid #2E86C1!important;}
.skin-blue .main-header .logo{font-family:'Barlow Condensed',sans-serif!important;font-size:18px!important;
  font-weight:700!important;letter-spacing:2px;color:#FFFFFF!important;}
.skin-blue .main-header .navbar .sidebar-toggle{color:#AED6F1!important;}
.skin-blue .main-sidebar{background-color:#1A2332!important;border-right:1px solid #253545!important;}
.skin-blue .sidebar-menu>li>a{color:#7F8C8D!important;font-family:'Source Sans 3',sans-serif;
  font-size:13px;font-weight:500;border-left:3px solid transparent;transition:all 0.2s ease;
  padding:10px 15px 10px 20px!important;}
.skin-blue .sidebar-menu>li.active>a,
.skin-blue .sidebar-menu>li>a:hover{color:#AED6F1!important;background-color:#253545!important;
  border-left:3px solid #2E86C1!important;}
.skin-blue .sidebar-menu>li>a .fa{color:#2E86C1!important;}
.skin-blue .sidebar{background-color:#1A2332!important;}
.content-wrapper{background-color:#F0F4F8!important;padding:18px!important;}
.box{background:#FFFFFF!important;border:1px solid #D5E8F3!important;border-top:3px solid #2E86C1!important;
  border-radius:6px!important;box-shadow:0 2px 8px rgba(27,79,114,0.08)!important;color:#1A2332!important;}
.box-header{background:transparent!important;border-bottom:1px solid #EBF5FB!important;padding:10px 15px!important;}
.box-title{font-family:'Barlow Condensed',sans-serif!important;font-size:15px!important;font-weight:600!important;
  letter-spacing:1px;color:#1B4F72!important;text-transform:uppercase;}
.info-box{background:#FFFFFF!important;border-radius:6px!important;border:1px solid #D5E8F3!important;
  box-shadow:0 2px 8px rgba(27,79,114,0.08)!important;min-height:75px;}
.info-box-icon{border-radius:6px 0 0 6px!important;}
.info-box-text{font-family:'Source Sans 3',sans-serif!important;font-size:11px!important;
  font-weight:600!important;text-transform:uppercase;letter-spacing:1.2px;color:#5D6D7E!important;}
.info-box-number{font-family:'Barlow Condensed',sans-serif!important;font-size:28px!important;
  font-weight:700!important;color:#1B4F72!important;}
.form-control{background-color:#F8FBFD!important;border:1px solid #AED6F1!important;
  color:#1A2332!important;border-radius:4px!important;font-family:'Source Sans 3',sans-serif;font-size:13px;}
.form-control:focus{border-color:#2E86C1!important;box-shadow:0 0 0 2px rgba(46,134,193,0.15)!important;outline:none!important;}
label{color:#5D6D7E!important;font-size:11px!important;font-weight:600!important;
  text-transform:uppercase;letter-spacing:1px;margin-bottom:4px;}
.btn-primary{background:linear-gradient(135deg,#2E86C1,#1B4F72)!important;border:none!important;
  border-radius:4px!important;font-family:'Barlow Condensed',sans-serif!important;font-weight:600!important;
  font-size:14px!important;letter-spacing:1.5px;padding:9px 22px!important;color:#fff!important;
  transition:all 0.2s ease!important;box-shadow:0 3px 10px rgba(46,134,193,0.3)!important;text-transform:uppercase;}
.btn-primary:hover{transform:translateY(-1px)!important;box-shadow:0 5px 14px rgba(46,134,193,0.4)!important;}
.page-header-banner{background:linear-gradient(135deg,#1B4F72 0%,#2E86C1 100%);border-radius:6px;
  padding:14px 20px;margin-bottom:18px;display:flex;align-items:center;gap:14px;
  box-shadow:0 3px 12px rgba(27,79,114,0.2);}
.page-header-title{font-family:'Barlow Condensed',sans-serif;font-size:22px;font-weight:700;
  color:#FFFFFF;letter-spacing:2px;text-transform:uppercase;}
.page-header-sub{font-size:12px;color:#AED6F1;margin-top:2px;letter-spacing:0.5px;}
.module-badge{background:rgba(255,255,255,0.15);border:1px solid rgba(255,255,255,0.3);
  border-radius:4px;padding:6px 14px;font-family:'Barlow Condensed',sans-serif;font-size:16px;
  font-weight:700;color:#fff;letter-spacing:3px;white-space:nowrap;}
.dataTables_wrapper{color:#1A2332!important;font-family:'Source Sans 3',sans-serif!important;font-size:12px!important;}
table.dataTable thead th{background:#EBF5FB!important;color:#1B4F72!important;
  border-bottom:2px solid #AED6F1!important;font-size:11px!important;
  text-transform:uppercase;letter-spacing:0.8px;font-weight:600!important;}
table.dataTable tbody tr{background:#FFFFFF!important;color:#1A2332!important;}
table.dataTable tbody tr:nth-child(even){background:#F8FBFD!important;}
table.dataTable tbody tr:hover{background:#EBF5FB!important;}
table.dataTable tbody td{border-color:#EBF5FB!important;}
.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select{background:#F8FBFD!important;color:#1A2332!important;border-color:#AED6F1!important;}
.dataTables_wrapper .dataTables_paginate .paginate_button.current{background:#2E86C1!important;color:#fff!important;border-color:#2E86C1!important;}
::-webkit-scrollbar{width:5px;}
::-webkit-scrollbar-track{background:#F0F4F8;}
::-webkit-scrollbar-thumb{background:#AED6F1;border-radius:3px;}
::-webkit-scrollbar-thumb:hover{background:#2E86C1;}
.selectize-input{background:#F8FBFD!important;border-color:#AED6F1!important;color:#1A2332!important;}
.selectize-dropdown{background:#FFFFFF!important;border-color:#AED6F1!important;color:#1A2332!important;}
"

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "TNSTC NETWORK OPT", titleWidth = 240),

  dashboardSidebar(width = 220,
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(id = "tabs",
      menuItem("Network Overview",    tabName = "overview",   icon = icon("project-diagram")),
      menuItem("Route Performance",   tabName = "routes",     icon = icon("route")),
      menuItem("Hub & Node Analysis", tabName = "nodes",      icon = icon("map-marker-alt")),
      menuItem("Load Factor",         tabName = "loadfactor", icon = icon("users")),
      menuItem("Transfer Analysis",   tabName = "transfers",  icon = icon("exchange-alt")),
      menuItem("Fleet Utilisation",   tabName = "fleet",      icon = icon("bus")),
      menuItem("Revenue & Cost",      tabName = "revcost",    icon = icon("chart-bar")),
      menuItem("Data Tables",         tabName = "datatables", icon = icon("table")),
      menuItem("About",               tabName = "about",      icon = icon("info-circle"))
    )
  ),

  dashboardBody(tabItems(

    # 1. NETWORK OVERVIEW
    tabItem(tabName = "overview",
      div(class="page-header-banner",
        div(class="module-badge","NET"),
        div(div(class="page-header-title","Network Overview"),
            div(class="page-header-sub","TNSTC Volvo · Tamil Nadu & Karnataka · Revenue Management"))),
      fluidRow(
        infoBoxOutput("ib_routes",    width=3), infoBoxOutput("ib_stations",  width=3),
        infoBoxOutput("ib_schedules", width=3), infoBoxOutput("ib_revenue",   width=3)),
      fluidRow(
        infoBoxOutput("ib_passengers",width=3), infoBoxOutput("ib_loadfactor",width=3),
        infoBoxOutput("ib_ontime",    width=3), infoBoxOutput("ib_profit",    width=3)),
      fluidRow(
        box(title="Network Map — Route Connections", width=8,
            plotlyOutput("plot_network_map", height="400px")),
        box(title="Route Profitability", width=2,
            plotlyOutput("plot_profit_dist", height="190px"), br(),
            plotlyOutput("plot_demand_dist", height="190px"))),
      fluidRow(
        box(title="Monthly Revenue & Passengers", width=8,
            plotlyOutput("plot_monthly_trend", height="260px")),
        box(title="Season Performance", width=4,
            plotlyOutput("plot_season_perf", height="260px")))
    ),

    # 2. ROUTE PERFORMANCE
    tabItem(tabName = "routes",
      div(class="page-header-banner",
        div(class="module-badge","RTE"),
        div(div(class="page-header-title","Route Performance Analysis"),
            div(class="page-header-sub","Profitability · Demand · Revenue/km · On-Time Rate"))),
      fluidRow(
        box(width=3,
          selectInput("rte_prof","Profitability Class",choices=c("All","High","Medium","Low")),
          selectInput("rte_demand","Demand Class",choices=c("All","High Demand","Moderate","Low Demand")),
          br(),
          div(style="background:#EBF5FB;border-radius:6px;padding:12px;border-left:4px solid #2E86C1;",
            div(style="font-family:'Barlow Condensed',sans-serif;font-size:28px;font-weight:700;color:#1B4F72;",
                textOutput("rte_total_routes")),
            div(style="font-size:11px;color:#5D6D7E;font-weight:600;text-transform:uppercase;letter-spacing:1px;",
                "Routes Shown")),
          br(),
          div(style="background:#EBF5FB;border-radius:6px;padding:12px;border-left:4px solid #1B4F72;",
            div(style="font-family:'Barlow Condensed',sans-serif;font-size:28px;font-weight:700;color:#1B4F72;",
                textOutput("rte_avg_occ")),
            div(style="font-size:11px;color:#5D6D7E;font-weight:600;text-transform:uppercase;letter-spacing:1px;",
                "Avg Occupancy"))
        ),
        box(title="Revenue per km by Route", width=9,
            plotlyOutput("plot_rev_per_km", height="380px"))),
      fluidRow(
        box(title="Profit Margin vs Occupancy Rate", width=6,
            plotlyOutput("plot_profit_occ", height="300px")),
        box(title="On-Time Rate by Route", width=6,
            plotlyOutput("plot_ontime_route", height="300px"))),
      fluidRow(
        box(title="Route Scorecard", width=12, DTOutput("tbl_route_score")))
    ),

    # 3. HUB & NODE
    tabItem(tabName = "nodes",
      div(class="page-header-banner",
        div(class="module-badge","HUB"),
        div(div(class="page-header-title","Hub & Node Analysis"),
            div(class="page-header-sub","Connectivity · Hub Score · Throughput · Platform Utilisation"))),
      fluidRow(
        box(title="Hub Score & Connectivity Degree", width=6,
            plotlyOutput("plot_hub_score", height="320px")),
        box(title="Passenger Throughput by Station", width=6,
            plotlyOutput("plot_throughput", height="320px"))),
      fluidRow(
        box(title="Station Type Classification", width=4,
            plotlyOutput("plot_node_type", height="260px")),
        box(title="Platform Utilisation Rate", width=8,
            plotlyOutput("plot_platform_util", height="260px"))),
      fluidRow(
        box(title="Node Metrics Table", width=12, DTOutput("tbl_node_metrics")))
    ),

    # 4. LOAD FACTOR
    tabItem(tabName = "loadfactor",
      div(class="page-header-banner",
        div(class="module-badge","LF"),
        div(div(class="page-header-title","Load Factor Analysis"),
            div(class="page-header-sub","Seat Utilisation · Capacity Planning · Demand Patterns"))),
      fluidRow(
        box(title="Load Factor Distribution", width=6,
            plotlyOutput("plot_lf_hist", height="300px")),
        box(title="Load Factor by Season", width=6,
            plotlyOutput("plot_lf_season", height="300px"))),
      fluidRow(
        box(title="Load Factor by Day of Week", width=6,
            plotlyOutput("plot_lf_dow", height="280px")),
        box(title="Load Factor vs Revenue per km", width=6,
            plotlyOutput("plot_lf_rev", height="280px"))),
      fluidRow(
        box(title="Load Factor Heatmap — Route x Season", width=12,
            plotlyOutput("plot_lf_heatmap", height="320px")))
    ),

    # 5. TRANSFER
    tabItem(tabName = "transfers",
      div(class="page-header-banner",
        div(class="module-badge","TRF"),
        div(div(class="page-header-title","Transfer & Connection Analysis"),
            div(class="page-header-sub","Hub Connections · Waiting Time · Fare Penalty · Connection Rate"))),
      fluidRow(
        infoBoxOutput("ib_tf_total",  width=3), infoBoxOutput("ib_tf_success",width=3),
        infoBoxOutput("ib_tf_wait",   width=3), infoBoxOutput("ib_tf_penalty", width=3)),
      fluidRow(
        box(title="Connection Success Rate by Hub", width=6,
            plotlyOutput("plot_tf_hub_success", height="300px")),
        box(title="Waiting Time Distribution", width=6,
            plotlyOutput("plot_tf_wait_dist", height="300px"))),
      fluidRow(
        box(title="Fare Penalty by Season", width=6,
            plotlyOutput("plot_tf_penalty_season", height="280px")),
        box(title="Transfer Volume by Season", width=6,
            plotlyOutput("plot_tf_volume_season", height="280px")))
    ),

    # 6. FLEET
    tabItem(tabName = "fleet",
      div(class="page-header-banner",
        div(class="module-badge","FLT"),
        div(div(class="page-header-title","Fleet Utilisation"),
            div(class="page-header-sub","Bus Type · Seat Utilisation · Cost Efficiency · Deployment"))),
      fluidRow(
        box(title="Trips by Bus Type", width=4,
            plotlyOutput("plot_fleet_trips", height="280px")),
        box(title="Load Factor by Bus Type", width=4,
            plotlyOutput("plot_fleet_lf", height="280px")),
        box(title="Revenue vs Cost by Bus Type", width=4,
            plotlyOutput("plot_fleet_revcost", height="280px"))),
      fluidRow(
        box(title="Departure Hour Distribution", width=6,
            plotlyOutput("plot_fleet_departures", height="280px")),
        box(title="Seats Available by Day of Week", width=6,
            plotlyOutput("plot_fleet_seats_dow", height="280px")))
    ),

    # 7. REVENUE & COST
    tabItem(tabName = "revcost",
      div(class="page-header-banner",
        div(class="module-badge","R&C"),
        div(div(class="page-header-title","Revenue & Cost Analytics"),
            div(class="page-header-sub","Profit Margins · Cost Breakdown · Booking Channel · Passenger Mix"))),
      fluidRow(
        box(title="Revenue, Cost & Profit — Top 10 Routes", width=8,
            plotlyOutput("plot_rc_grouped", height="340px")),
        box(title="Booking Channel Revenue", width=4,
            plotlyOutput("plot_rc_channel", height="340px"))),
      fluidRow(
        box(title="Passenger Type Revenue Mix", width=4,
            plotlyOutput("plot_rc_passtype", height="280px")),
        box(title="Advance Booking Days vs Net Revenue", width=8,
            plotlyOutput("plot_rc_advance", height="280px")))
    ),

    # 8. DATA TABLES
    tabItem(tabName = "datatables",
      div(class="page-header-banner",
        div(class="module-badge","DATA"),
        div(div(class="page-header-title","Data Explorer"),
            div(class="page-header-sub","Browse all 5 dataset tables"))),
      fluidRow(box(width=12,
        tabsetPanel(
          tabPanel("Schedules", br(), DTOutput("dt_schedules")),
          tabPanel("Bookings",  br(), DTOutput("dt_bookings")),
          tabPanel("Routes",    br(), DTOutput("dt_routes")),
          tabPanel("Nodes",     br(), DTOutput("dt_nodes")),
          tabPanel("Transfers", br(), DTOutput("dt_transfers")))))
    ),

    # 9. ABOUT
    tabItem(tabName = "about",
      div(class="page-header-banner",
        div(class="module-badge","INFO"),
        div(div(class="page-header-title","About This Project"),
            div(class="page-header-sub","Network Optimization · Revenue Management · TNSTC Volvo Buses"))),
      fluidRow(
        box(title="Project Details", width=6,
          tags$div(style="font-family:'Source Sans 3',sans-serif;font-size:13px;line-height:1.9;color:#1A2332;",
            tags$p(tags$b(style="color:#1B4F72;","Subject: "), "Revenue Management"),
            tags$p(tags$b(style="color:#1B4F72;","Topic: "), "Network Optimization for TNSTC Volvo Buses"),
            tags$p(tags$b(style="color:#1B4F72;","Operator: "), "Tamil Nadu State Transport Corporation (TNSTC)"),
            tags$p(tags$b(style="color:#1B4F72;","Fleet: "), "Volvo AC Sleeper, Semi-Sleeper, Seater"),
            tags$hr(style="border-color:#D5E8F3;"),
            tags$p(style="color:#5D6D7E;","This app analyses network-level performance across 21 routes and 11 stations, focusing on route profitability, hub connectivity, load factor optimisation, and transfer efficiency."))),
        box(title="Key Network Metrics", width=6,
          tags$table(style="width:100%;border-collapse:collapse;font-size:13px;",
            tags$thead(tags$tr(
              tags$th(style="padding:8px;color:#1B4F72;border-bottom:2px solid #D5E8F3;font-weight:600;","Metric"),
              tags$th(style="padding:8px;color:#1B4F72;border-bottom:2px solid #D5E8F3;font-weight:600;","Purpose"))),
            tags$tbody(
              tags$tr(tags$td(style="padding:7px;color:#2E86C1;font-weight:600;","Load Factor"),
                      tags$td(style="padding:7px;","Seat utilisation efficiency")),
              tags$tr(style="background:#F8FBFD;",
                      tags$td(style="padding:7px;color:#2E86C1;font-weight:600;","Hub Score"),
                      tags$td(style="padding:7px;","Network centrality of each station")),
              tags$tr(tags$td(style="padding:7px;color:#2E86C1;font-weight:600;","Revenue / km"),
                      tags$td(style="padding:7px;","Route productivity measure")),
              tags$tr(style="background:#F8FBFD;",
                      tags$td(style="padding:7px;color:#2E86C1;font-weight:600;","Connectivity Degree"),
                      tags$td(style="padding:7px;","Number of links per node")),
              tags$tr(tags$td(style="padding:7px;color:#2E86C1;font-weight:600;","Transfer Success Rate"),
                      tags$td(style="padding:7px;","Connection reliability at hubs")),
              tags$tr(style="background:#F8FBFD;",
                      tags$td(style="padding:7px;color:#2E86C1;font-weight:600;","On-Time Rate"),
                      tags$td(style="padding:7px;","Network schedule reliability")),
              tags$tr(tags$td(style="padding:7px;color:#2E86C1;font-weight:600;","Cost per Seat-km"),
                      tags$td(style="padding:7px;","Operational cost efficiency")))))
      )
    )
  ))
)

# ---------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------
server <- function(input, output, session) {

  pc <- function(p) {
    p %>% plotly::layout(
      paper_bgcolor="#FFFFFF", plot_bgcolor="#FFFFFF",
      font=list(color="#5D6D7E", family="Source Sans 3"),
      xaxis=list(gridcolor="#EBF5FB", zerolinecolor="#D5E8F3"),
      yaxis=list(gridcolor="#EBF5FB", zerolinecolor="#D5E8F3"),
      margin=list(t=30,b=40,l=50,r=20)
    ) %>% plotly::config(displayModeBar=FALSE)
  }

  blues <- c("#1B4F72","#2E86C1","#5DADE2","#85C1E9","#AED6F1","#D6EAF8","#154360","#21618C")

  # --- INFO BOXES ---
  output$ib_routes    <- renderInfoBox(infoBox("Routes",    nrow(edges_master),icon=icon("route"),color="blue",fill=TRUE))
  output$ib_stations  <- renderInfoBox(infoBox("Stations",  nrow(station_coords),icon=icon("map-marker"),color="navy",fill=TRUE))
  output$ib_schedules <- renderInfoBox(infoBox("Schedules", nrow(schedule_df),icon=icon("calendar"),color="light-blue",fill=TRUE))
  output$ib_revenue   <- renderInfoBox(infoBox("Revenue",
    paste0("Rs ",formatC(round(sum(schedule_df$total_revenue_inr)/1e5,1),format="f",digits=1),"L"),
    icon=icon("rupee-sign"),color="green",fill=TRUE))
  output$ib_passengers <- renderInfoBox(infoBox("Passengers",
    formatC(sum(schedule_df$passengers_booked),format="d",big.mark=","),icon=icon("users"),color="blue",fill=TRUE))
  output$ib_loadfactor <- renderInfoBox(infoBox("Avg Load Factor",
    paste0(round(mean(schedule_df$occupancy_rate)*100,1),"%"),icon=icon("percent"),color="navy",fill=TRUE))
  output$ib_ontime    <- renderInfoBox(infoBox("On-Time Rate",
    paste0(round(mean(schedule_df$on_time)*100,1),"%"),icon=icon("clock"),color="light-blue",fill=TRUE))
  output$ib_profit    <- renderInfoBox(infoBox("Avg Profit Margin",
    paste0(round(mean(schedule_df$profit_margin_pct),1),"%"),icon=icon("chart-line"),color="green",fill=TRUE))

  # --- NETWORK MAP ---
  output$plot_network_map <- renderPlotly({
    p <- plot_ly()
    # Draw edges
    for (i in seq_len(nrow(edges_master))) {
      o_row <- station_coords[station_coords$station == edges_master$origin[i],]
      d_row <- station_coords[station_coords$station == edges_master$destination[i],]
      if (nrow(o_row)==0 || nrow(d_row)==0) next
      rp <- route_perf %>% filter(origin==edges_master$origin[i], destination==edges_master$destination[i])
      lwd <- if (nrow(rp)>0 && !is.na(rp$avg_occupancy_rate[1]) && rp$avg_occupancy_rate[1]>=0.75) 2.5 else 1.2
      col <- if (nrow(rp)>0 && !is.na(rp$avg_occupancy_rate[1]) && rp$avg_occupancy_rate[1]>=0.75) "#2E86C1" else "#AED6F1"
      p <- p %>% add_trace(type="scatter", mode="lines",
        x=c(o_row$lon, d_row$lon, NA), y=c(o_row$lat, d_row$lat, NA),
        line=list(width=lwd, color=col), hoverinfo="none", showlegend=FALSE)
    }
    # Draw nodes
    nd_col  <- ifelse(station_coords$node_type=="Hub","#1B4F72",
               ifelse(station_coords$node_type=="Intermediate","#2E86C1","#AED6F1"))
    nd_size <- ifelse(station_coords$node_type=="Hub",18,
               ifelse(station_coords$node_type=="Intermediate",13,10))
    hs      <- node_metrics$hub_score[match(station_coords$station, node_metrics$station)]
    hs[is.na(hs)] <- 0
    p <- p %>% add_trace(type="scatter", mode="markers+text",
      x=station_coords$lon, y=station_coords$lat,
      text=station_coords$station,
      textposition="top center",
      textfont=list(size=8, color="#1B4F72", family="Barlow Condensed"),
      marker=list(size=nd_size, color=nd_col, line=list(color="#FFFFFF",width=2)),
      hovertext=paste0("<b>",station_coords$station,"</b><br>Type: ",station_coords$node_type,"<br>Hub Score: ",hs),
      hoverinfo="text", showlegend=FALSE)
    p %>% layout(
      paper_bgcolor="#F8FBFD", plot_bgcolor="#F8FBFD",
      xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE,title=""),
      yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE,title=""),
      margin=list(t=10,b=10,l=10,r=10), showlegend=FALSE
    ) %>% config(displayModeBar=FALSE)
  })

  output$plot_profit_dist <- renderPlotly({
    df <- route_perf %>% count(profitability_class)
    pc(plot_ly(df, labels=~profitability_class, values=~n, type="pie", hole=0.5,
               marker=list(colors=blues[1:3], line=list(color="#FFFFFF",width=2)),
               textfont=list(color="#FFFFFF",size=10)) %>%
       layout(title=list(text="Profit Class",font=list(size=11,color="#1B4F72")),showlegend=TRUE,
              legend=list(font=list(size=9,color="#5D6D7E"))))
  })

  output$plot_demand_dist <- renderPlotly({
    df <- route_perf %>% count(demand_class)
    pc(plot_ly(df, labels=~demand_class, values=~n, type="pie", hole=0.5,
               marker=list(colors=blues[2:4], line=list(color="#FFFFFF",width=2)),
               textfont=list(color="#FFFFFF",size=10)) %>%
       layout(title=list(text="Demand Class",font=list(size=11,color="#1B4F72")),showlegend=TRUE,
              legend=list(font=list(size=9,color="#5D6D7E"))))
  })

  output$plot_monthly_trend <- renderPlotly({
    df <- schedule_df %>%
      mutate(month=floor_date(travel_date,"month")) %>%
      group_by(month) %>%
      summarise(revenue=sum(total_revenue_inr)/1000, passengers=sum(passengers_booked), .groups="drop")
    pc(plot_ly(df,x=~month) %>%
      add_trace(y=~revenue, type="bar", name="Revenue (Rs K)", marker=list(color="#AED6F1")) %>%
      add_trace(y=~passengers, type="scatter", mode="lines+markers", name="Passengers",
                line=list(color="#1B4F72",width=2.5), marker=list(color="#2E86C1",size=7), yaxis="y2") %>%
      layout(yaxis=list(title="Revenue (Rs 000)"),
             yaxis2=list(title="Passengers",overlaying="y",side="right"),
             legend=list(orientation="h",y=1.12)))
  })

  output$plot_season_perf <- renderPlotly({
    df <- schedule_df %>% group_by(season) %>%
      summarise(avg_lf=round(mean(occupancy_rate)*100,1),
                avg_margin=round(mean(profit_margin_pct),1), .groups="drop")
    pc(plot_ly(df,x=~season) %>%
      add_trace(y=~avg_lf, type="bar", name="Load Factor (%)", marker=list(color="#2E86C1")) %>%
      add_trace(y=~avg_margin, type="scatter", mode="lines+markers", name="Profit Margin (%)",
                line=list(color="#1B4F72",width=2), marker=list(size=7,color="#1B4F72"), yaxis="y2") %>%
      layout(yaxis=list(title="Load Factor (%)"),
             yaxis2=list(title="Profit Margin (%)",overlaying="y",side="right"),
             legend=list(orientation="h",y=1.12)))
  })

  # --- ROUTE PERFORMANCE ---
  filtered_routes <- reactive({
    df <- route_perf
    if (!is.null(input$rte_prof)   && input$rte_prof   != "All") df <- df %>% filter(profitability_class==input$rte_prof)
    if (!is.null(input$rte_demand) && input$rte_demand != "All") df <- df %>% filter(demand_class==input$rte_demand)
    df
  })

  output$rte_total_routes <- renderText(nrow(filtered_routes()))
  output$rte_avg_occ      <- renderText({
    v <- mean(filtered_routes()$avg_occupancy_rate, na.rm=TRUE)
    paste0(round(v*100,1),"%")
  })

  output$plot_rev_per_km <- renderPlotly({
    df <- filtered_routes() %>% arrange(desc(avg_revenue_per_km)) %>%
      mutate(rs=paste0(substr(origin,1,5),"->",substr(destination,1,5)))
    cols <- ifelse(df$profitability_class=="High","#1B4F72",
            ifelse(df$profitability_class=="Medium","#2E86C1","#AED6F1"))
    pc(plot_ly(df, x=~reorder(rs,avg_revenue_per_km), y=~avg_revenue_per_km,
               type="bar", marker=list(color=cols),
               hovertext=paste0("<b>",df$route_label,"</b><br>Rev/km: Rs",df$avg_revenue_per_km,
                                "<br>Class: ",df$profitability_class), hoverinfo="text") %>%
       layout(xaxis=list(title="",tickangle=-40,tickfont=list(size=9)),
              yaxis=list(title="Revenue per km (Rs)"),showlegend=FALSE))
  })

  output$plot_profit_occ <- renderPlotly({
    df <- filtered_routes()
    pc(plot_ly(df, x=~avg_occupancy_rate, y=~avg_profit_margin,
               type="scatter", mode="markers+text",
               text=~paste0(substr(origin,1,4),"->",substr(destination,1,4)),
               textposition="top right", textfont=list(size=8,color="#1B4F72"),
               marker=list(size=12, color=~avg_revenue_per_km,
                           colorscale=list(c(0,"#D6EAF8"),c(0.5,"#2E86C1"),c(1,"#1B4F72")),
                           showscale=TRUE,
                           colorbar=list(title="Rev/km",tickfont=list(size=9))),
               hovertext=paste0("<b>",df$route_label,"</b><br>Occ: ",
                                round(df$avg_occupancy_rate*100,1),"%<br>Margin: ",df$avg_profit_margin,"%"),
               hoverinfo="text") %>%
       layout(xaxis=list(title="Avg Occupancy Rate",tickformat=".0%"),
              yaxis=list(title="Avg Profit Margin (%)")))
  })

  output$plot_ontime_route <- renderPlotly({
    df <- filtered_routes() %>% arrange(on_time_rate) %>%
      mutate(rs=paste0(substr(origin,1,5),"->",substr(destination,1,5)))
    cols <- ifelse(df$on_time_rate>=0.9,"#1B4F72",ifelse(df$on_time_rate>=0.8,"#2E86C1","#AED6F1"))
    pc(plot_ly(df, x=~on_time_rate, y=~reorder(rs,on_time_rate), type="bar", orientation="h",
               marker=list(color=cols),
               text=paste0(round(df$on_time_rate*100,1),"%"), textposition="outside",
               textfont=list(size=9,color="#1B4F72")) %>%
       layout(xaxis=list(title="On-Time Rate",tickformat=".0%"),
              yaxis=list(title="",tickfont=list(size=8)),showlegend=FALSE))
  })

  output$tbl_route_score <- renderDT({
    df <- filtered_routes() %>%
      select(route_label,distance_km,avg_occupancy_rate,avg_fare_inr,
             avg_profit_margin,avg_revenue_per_km,on_time_rate,
             profitability_class,demand_class) %>%
      mutate(avg_occupancy_rate=paste0(round(avg_occupancy_rate*100,1),"%"),
             on_time_rate=paste0(round(on_time_rate*100,1),"%"),
             avg_profit_margin=paste0(avg_profit_margin,"%"))
    datatable(df, rownames=FALSE,
              colnames=c("Route","Dist km","Occupancy","Avg Fare","Profit%","Rev/km","OnTime%","Profit Class","Demand"),
              options=list(pageLength=10,scrollX=TRUE,
                           columnDefs=list(list(className="dt-center",targets="_all"))))
  })

  # --- HUB & NODE ---
  output$plot_hub_score <- renderPlotly({
    df <- node_metrics %>% arrange(desc(hub_score))
    cols <- ifelse(df$node_type=="Hub","#1B4F72",ifelse(df$node_type=="Intermediate","#2E86C1","#AED6F1"))
    pc(plot_ly(df, x=~reorder(station,hub_score), y=~hub_score, type="bar",
               marker=list(color=cols),
               text=~connectivity_degree, textposition="outside",
               textfont=list(size=10,color="#1B4F72"),
               hovertext=paste0("<b>",df$station,"</b><br>Hub Score: ",df$hub_score,
                                "<br>Degree: ",df$connectivity_degree,"<br>Type: ",df$node_type),
               hoverinfo="text") %>%
       layout(xaxis=list(title="",tickangle=-30,tickfont=list(size=9)),
              yaxis=list(title="Hub Score (0-100)"),showlegend=FALSE))
  })

  output$plot_throughput <- renderPlotly({
    df <- node_metrics %>% arrange(desc(total_throughput))
    pc(plot_ly(df, x=~reorder(station,total_throughput)) %>%
      add_trace(y=~departing_passengers, type="bar", name="Departing", marker=list(color="#1B4F72")) %>%
      add_trace(y=~arriving_passengers,  type="bar", name="Arriving",  marker=list(color="#AED6F1")) %>%
      layout(barmode="stack",
             xaxis=list(title="",tickangle=-30,tickfont=list(size=9)),
             yaxis=list(title="Passengers"),
             legend=list(orientation="h",y=1.1)))
  })

  output$plot_node_type <- renderPlotly({
    df <- node_metrics %>% count(node_type)
    pc(plot_ly(df, labels=~node_type, values=~n, type="pie", hole=0.45,
               marker=list(colors=blues[1:3], line=list(color="#FFFFFF",width=2)),
               textfont=list(color="#FFFFFF",size=11)))
  })

  output$plot_platform_util <- renderPlotly({
    df <- node_metrics %>% arrange(desc(avg_platform_util))
    cols <- ifelse(df$avg_platform_util>=0.7,"#1B4F72","#AED6F1")
    pc(plot_ly(df, x=~reorder(station,avg_platform_util), y=~avg_platform_util,
               type="scatter", mode="lines+markers",
               line=list(color="#2E86C1",width=2),
               marker=list(size=10, color=cols, line=list(color="#FFFFFF",width=2)),
               text=paste0(round(df$avg_platform_util*100,1),"%"),
               textposition="top", textfont=list(size=9,color="#1B4F72"),
               hovertext=paste0(df$station,"<br>Util: ",round(df$avg_platform_util*100,1),"%"),
               hoverinfo="text") %>%
       layout(xaxis=list(title="",tickangle=-30,tickfont=list(size=9)),
              yaxis=list(title="Platform Utilisation Rate",tickformat=".0%"),showlegend=FALSE))
  })

  output$tbl_node_metrics <- renderDT({
    df <- node_metrics %>%
      select(station,node_type,connectivity_degree,hub_score,
             departing_trips,arriving_trips,total_throughput,avg_platform_util) %>%
      mutate(avg_platform_util=paste0(round(avg_platform_util*100,1),"%"))
    datatable(df, rownames=FALSE,
              options=list(pageLength=11,scrollX=TRUE,
                           columnDefs=list(list(className="dt-center",targets="_all"))))
  })

  # --- LOAD FACTOR ---
  output$plot_lf_hist <- renderPlotly({
    pc(plot_ly(x=~schedule_df$occupancy_rate*100, type="histogram", nbinsx=20,
               marker=list(color="#2E86C1", line=list(color="#FFFFFF",width=0.8))) %>%
       layout(xaxis=list(title="Load Factor (%)"),yaxis=list(title="Frequency")))
  })

  output$plot_lf_season <- renderPlotly({
    df <- schedule_df %>% group_by(season) %>%
      summarise(avg_lf=mean(occupancy_rate)*100, sd_lf=sd(occupancy_rate)*100, .groups="drop")
    pc(plot_ly(df, x=~season, y=~avg_lf, type="bar",
               error_y=list(type="data",array=~sd_lf,color="#1B4F72"),
               marker=list(color=blues[seq_len(nrow(df))]),
               text=paste0(round(df$avg_lf,1),"%"), textposition="outside",
               textfont=list(size=10,color="#1B4F72")) %>%
       layout(xaxis=list(title=""),yaxis=list(title="Avg Load Factor (%)"),showlegend=FALSE))
  })

  output$plot_lf_dow <- renderPlotly({
    dow_order <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    df <- schedule_df %>% group_by(day_of_week) %>%
      summarise(avg_lf=mean(occupancy_rate)*100, .groups="drop") %>%
      mutate(day_of_week=factor(day_of_week,levels=dow_order)) %>% arrange(day_of_week)
    cols <- ifelse(df$day_of_week %in% c("Saturday","Sunday"),"#1B4F72","#AED6F1")
    pc(plot_ly(df, x=~day_of_week, y=~avg_lf, type="bar", marker=list(color=cols),
               text=paste0(round(df$avg_lf,1),"%"), textposition="outside",
               textfont=list(size=10,color="#1B4F72")) %>%
       layout(xaxis=list(title=""),yaxis=list(title="Avg Load Factor (%)"),showlegend=FALSE))
  })

  output$plot_lf_rev <- renderPlotly({
    samp <- schedule_df %>% sample_n(min(400,nrow(schedule_df)))
    pc(plot_ly(samp, x=~occupancy_rate, y=~revenue_per_km, color=~season,
               type="scatter", mode="markers",
               colors=blues[1:5], marker=list(size=6,opacity=0.65)) %>%
       layout(xaxis=list(title="Load Factor",tickformat=".0%"),
              yaxis=list(title="Revenue per km (Rs)"),
              legend=list(orientation="h",y=1.1)))
  })

  output$plot_lf_heatmap <- renderPlotly({
    df <- schedule_df %>%
      mutate(rs=paste0(substr(origin,1,5),"->",substr(destination,1,5))) %>%
      group_by(rs, season) %>%
      summarise(avg_lf=round(mean(occupancy_rate)*100,1), .groups="drop")
    mat <- pivot_wider(df, names_from=season, values_from=avg_lf, values_fill=0)
    rn  <- mat$rs; mat <- as.matrix(mat[,-1])
    pc(plot_ly(z=mat, x=colnames(mat), y=rn, type="heatmap",
               colorscale=list(c(0,"#D6EAF8"),c(0.5,"#2E86C1"),c(1,"#1B4F72")),
               hovertemplate="Route: %{y}<br>Season: %{x}<br>LF: %{z}%<extra></extra>") %>%
       layout(xaxis=list(title="Season",tickfont=list(size=10)),
              yaxis=list(title="",tickfont=list(size=8))))
  })

  # --- TRANSFERS ---
  output$ib_tf_total   <- renderInfoBox(infoBox("Total Transfers", nrow(transfer_df),icon=icon("exchange-alt"),color="blue",fill=TRUE))
  output$ib_tf_success <- renderInfoBox(infoBox("Connection Rate",paste0(round(mean(transfer_df$connection_made)*100,1),"%"),icon=icon("check-circle"),color="green",fill=TRUE))
  output$ib_tf_wait    <- renderInfoBox(infoBox("Avg Wait Time",paste0(round(mean(transfer_df$waiting_time_min),0)," min"),icon=icon("clock"),color="navy",fill=TRUE))
  output$ib_tf_penalty <- renderInfoBox(infoBox("Avg Fare Penalty",paste0("Rs ",round(mean(transfer_df$fare_penalty_inr),0)),icon=icon("tag"),color="light-blue",fill=TRUE))

  output$plot_tf_hub_success <- renderPlotly({
    df <- transfer_df %>% group_by(transfer_hub) %>%
      summarise(success_rate=round(mean(connection_made)*100,1), .groups="drop")
    cols <- ifelse(df$success_rate>=90,"#1B4F72",ifelse(df$success_rate>=80,"#2E86C1","#AED6F1"))
    pc(plot_ly(df, x=~reorder(transfer_hub,success_rate), y=~success_rate, type="bar",
               marker=list(color=cols),
               text=paste0(df$success_rate,"%"), textposition="outside",
               textfont=list(size=10,color="#1B4F72")) %>%
       layout(xaxis=list(title="",tickangle=-20,tickfont=list(size=9)),
              yaxis=list(title="Connection Success Rate (%)"),showlegend=FALSE))
  })

  output$plot_tf_wait_dist <- renderPlotly({
    pc(plot_ly(x=~transfer_df$waiting_time_min, type="histogram", nbinsx=25,
               marker=list(color="#2E86C1", line=list(color="#FFFFFF",width=0.8))) %>%
       layout(xaxis=list(title="Waiting Time (minutes)"),yaxis=list(title="Frequency")))
  })

  output$plot_tf_penalty_season <- renderPlotly({
    df <- transfer_df %>% group_by(season) %>%
      summarise(avg_penalty=round(mean(fare_penalty_inr),1), .groups="drop")
    pc(plot_ly(df, x=~season, y=~avg_penalty, type="bar",
               marker=list(color=blues[seq_len(nrow(df))]),
               text=paste0("Rs ",df$avg_penalty), textposition="outside",
               textfont=list(size=10,color="#1B4F72")) %>%
       layout(xaxis=list(title=""),yaxis=list(title="Avg Fare Penalty (Rs)"),showlegend=FALSE))
  })

  output$plot_tf_volume_season <- renderPlotly({
    df <- transfer_df %>% count(season)
    pc(plot_ly(df, x=~season, y=~n, type="bar",
               marker=list(color=blues[seq_len(nrow(df))])) %>%
       layout(xaxis=list(title=""),yaxis=list(title="Transfer Count"),showlegend=FALSE))
  })

  # --- FLEET ---
  output$plot_fleet_trips <- renderPlotly({
    df <- schedule_df %>% count(bus_type)
    pc(plot_ly(df, labels=~bus_type, values=~n, type="pie", hole=0.45,
               marker=list(colors=blues[1:3], line=list(color="#FFFFFF",width=2)),
               textfont=list(color="#FFFFFF",size=10)))
  })

  output$plot_fleet_lf <- renderPlotly({
    df <- schedule_df %>% group_by(bus_type) %>%
      summarise(avg_lf=round(mean(occupancy_rate)*100,1), .groups="drop")
    pc(plot_ly(df, x=~bus_type, y=~avg_lf, type="bar",
               marker=list(color=blues[1:3]),
               text=paste0(df$avg_lf,"%"), textposition="outside",
               textfont=list(size=11,color="#1B4F72")) %>%
       layout(xaxis=list(title=""),yaxis=list(title="Avg Load Factor (%)"),showlegend=FALSE))
  })

  output$plot_fleet_revcost <- renderPlotly({
    df <- schedule_df %>% group_by(bus_type) %>%
      summarise(rev=sum(total_revenue_inr)/1000, cost=sum(total_cost_inr)/1000, .groups="drop")
    pc(plot_ly(df, x=~bus_type) %>%
      add_trace(y=~rev,  type="bar", name="Revenue", marker=list(color="#2E86C1")) %>%
      add_trace(y=~cost, type="bar", name="Cost",    marker=list(color="#AED6F1")) %>%
      layout(barmode="group", xaxis=list(title=""),yaxis=list(title="Amount (Rs 000)"),
             legend=list(orientation="h",y=1.1)))
  })

  output$plot_fleet_departures <- renderPlotly({
    df <- schedule_df %>% count(departure_hour)
    pc(plot_ly(df, x=~departure_hour, y=~n, type="bar",
               marker=list(color="#2E86C1", line=list(color="#FFFFFF",width=0.8))) %>%
       layout(xaxis=list(title="Departure Hour (24h)"),yaxis=list(title="No. of Trips"),showlegend=FALSE))
  })

  output$plot_fleet_seats_dow <- renderPlotly({
    dow_order <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    df <- schedule_df %>% group_by(day_of_week) %>%
      summarise(avg_seats=round(mean(seats_available),1), .groups="drop") %>%
      mutate(day_of_week=factor(day_of_week,levels=dow_order)) %>% arrange(day_of_week)
    pc(plot_ly(df, x=~day_of_week, y=~avg_seats, type="scatter", mode="lines+markers",
               line=list(color="#1B4F72",width=2.5),
               marker=list(size=9,color="#2E86C1",line=list(color="#FFFFFF",width=2))) %>%
       layout(xaxis=list(title=""),yaxis=list(title="Avg Seats Available"),showlegend=FALSE))
  })

  # --- REVENUE & COST ---
  output$plot_rc_grouped <- renderPlotly({
    df <- route_perf %>% arrange(desc(total_revenue_inr)) %>% head(10) %>%
      mutate(rs=paste0(substr(origin,1,5),"->",substr(destination,1,5)))
    pc(plot_ly(df, x=~reorder(rs,total_profit_inr)) %>%
      add_trace(y=~total_revenue_inr/1000, type="bar", name="Revenue", marker=list(color="#2E86C1")) %>%
      add_trace(y=~total_cost_inr/1000,    type="bar", name="Cost",    marker=list(color="#AED6F1")) %>%
      add_trace(y=~total_profit_inr/1000,  type="bar", name="Profit",  marker=list(color="#1B4F72")) %>%
      layout(barmode="group",
             xaxis=list(title="",tickangle=-35,tickfont=list(size=9)),
             yaxis=list(title="Amount (Rs 000)"),
             legend=list(orientation="h",y=1.1)))
  })

  output$plot_rc_channel <- renderPlotly({
    df <- booking_df %>% group_by(booking_channel) %>%
      summarise(total=sum(net_revenue_inr)/1000, .groups="drop")
    pc(plot_ly(df, labels=~booking_channel, values=~total, type="pie", hole=0.45,
               marker=list(colors=blues[1:4], line=list(color="#FFFFFF",width=2)),
               textfont=list(color="#FFFFFF",size=10)))
  })

  output$plot_rc_passtype <- renderPlotly({
    df <- booking_df %>% group_by(passenger_type) %>%
      summarise(total=sum(net_revenue_inr)/1000, .groups="drop")
    pc(plot_ly(df, x=~passenger_type, y=~total, type="bar",
               marker=list(color=blues[1:4])) %>%
       layout(xaxis=list(title=""),yaxis=list(title="Revenue (Rs 000)"),showlegend=FALSE))
  })

  output$plot_rc_advance <- renderPlotly({
    df <- booking_df %>%
      mutate(adv_grp=cut(days_in_advance,breaks=c(-1,1,3,7,14,30),
                         labels=c("0-1","2-3","4-7","8-14","15-30"))) %>%
      group_by(adv_grp) %>%
      summarise(avg_rev=round(mean(net_revenue_inr),1), count=n(), .groups="drop")
    pc(plot_ly(df, x=~adv_grp) %>%
      add_trace(y=~avg_rev, type="bar", name="Avg Net Revenue (Rs)", marker=list(color="#2E86C1")) %>%
      add_trace(y=~count, type="scatter", mode="lines+markers", name="Bookings",
                line=list(color="#1B4F72",width=2), marker=list(size=8,color="#1B4F72"), yaxis="y2") %>%
      layout(yaxis=list(title="Avg Net Revenue (Rs)"),
             yaxis2=list(title="Booking Count",overlaying="y",side="right"),
             xaxis=list(title="Days in Advance"),
             legend=list(orientation="h",y=1.1)))
  })

  # --- DATA TABLES ---
  dt_opts <- list(pageLength=10, scrollX=TRUE,
                  columnDefs=list(list(className="dt-center",targets="_all")))

  output$dt_schedules <- renderDT(datatable(
    schedule_df %>% select(schedule_id,travel_date,origin,destination,season,
                           bus_type,total_seats,passengers_booked,occupancy_rate,
                           actual_fare_inr,total_revenue_inr,profit_inr,on_time),
    rownames=FALSE, options=dt_opts))

  output$dt_bookings <- renderDT(datatable(
    booking_df %>% select(booking_id,schedule_id,origin,destination,travel_date,
                          season,bus_type,days_in_advance,booking_channel,
                          seat_class,passenger_type,fare_paid_inr,cancellation,net_revenue_inr),
    rownames=FALSE, options=dt_opts))

  output$dt_routes <- renderDT(datatable(
    route_perf %>% select(route_label,distance_km,total_schedules,avg_occupancy_rate,
                          avg_fare_inr,total_revenue_inr,avg_profit_margin,
                          avg_revenue_per_km,on_time_rate,profitability_class,demand_class),
    rownames=FALSE, options=dt_opts))

  output$dt_nodes <- renderDT(datatable(
    node_metrics %>% select(station,node_type,connectivity_degree,hub_score,
                            departing_trips,arriving_trips,total_throughput,avg_platform_util),
    rownames=FALSE, options=dt_opts))

  output$dt_transfers <- renderDT(datatable(
    transfer_df %>% select(transfer_id,travel_date,origin,transfer_hub,
                           final_destination,season,waiting_time_min,connection_made,
                           leg1_fare_inr,leg2_fare_inr,total_fare_inr,fare_penalty_inr),
    rownames=FALSE, options=dt_opts))
}

shinyApp(ui = ui, server = server)
