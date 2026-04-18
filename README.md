# 🚌 TNSTC Network Optimization App

An interactive R Shiny dashboard analyzing the TNSTC Volvo bus 
network across Tamil Nadu — covering route performance, schedules,
occupancy patterns, and transfer optimization.

## 🚨 Business Problem
TNSTC operates a complex multi-route Volvo network but lacks 
visibility into which routes underperform, when occupancy drops,
and where transfer connections can be optimized.

## 🔧 What This App Does
- **Network Overview** — visualizes bus routes across Tamil Nadu nodes
- **Route Performance** — compares revenue, occupancy & efficiency per route
- **Schedule Analysis** — departure time patterns and seasonal demand
- **Transfer Analysis** — identifies connection opportunities between routes
- **Data Explorer** — full booking and schedule dataset

## 📊 Technical Stack
R, Shiny, shinydashboard, Plotly, DT, dplyr, lubridate, tidyr

## 🗂️ Files
| File | Description |
|------|-------------|
| `tnstc_network_app.R` | Main Shiny app code |
| `tnstc_network_schedules.csv` | Schedule data |
| `tnstc_network_bookings.csv` | Booking data |
| `tnstc_network_route_perf.csv` | Route performance metrics |
| `tnstc_network_nodes.csv` | Network node data |
| `tnstc_network_transfers.csv` | Transfer connection data |
## 📱 App Preview
![App Demo](tnstc-network-demo.gif)
