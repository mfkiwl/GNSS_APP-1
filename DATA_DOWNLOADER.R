library(RGtk2)
library(httr)
library(wkb)

request.url = "http://112.137.134.7:5000/data/inTime?"
station.rqurl ="http://112.137.134.7:5000/station"

s = GET(station.rqurl)
stations <- content(s, as="parsed")
combo <- gtkComboBoxNewText()
combo$appendText(stations[[1]]$name)
combo$appendText(stations[[2]]$name)

get.data <- function(station.id, start.time, n) {
  request.str = paste0(
    request.url,
    "stationID=",
    station.id,
    "&start=",
    start.time,
    "&time=",
    n,
    "&separate=false"
  )
  
  data.df <- content(GET(request.str))$data
  return(data.df)
}

to.GPS.timestamp <- function(year, month, day, hour, min, sec) {
  return(as.numeric(strptime(paste(year, "-", month, "-", day, " ", hour, ":", min, ":", sec, sep = ""),format="%Y-%m-%d %H:%M:%OS",tz = "+0")) - 315964782)
}



gtk.Calenda = gtkCalendar(show=TRUE)
gtk.path = gtkFileChooserButton(title = "Select where to save data",
                                action="select-folder")
gtk.button = gtkButton(label="Pull")
gtk.text = gtkTextView()

window <- gtkWindow("toplevel")
window["title"] <- "GNSS data downloader"
window$setDefaultSize(200, 200)

box <- gtkVBox(FALSE, 5)
box$packStart(combo, fill =F)
box$packStart(gtk.Calenda, fill=F)
box$packStart(gtk.path, fill=F)
box$packStart(gtk.button, fill=F)
box$packStart(gtk.text)
window$add(box)
tv.buf = gtkTextViewGetBuffer(gtk.text)

pull <- function(btn, ...) {
  date = gtkCalendarGetDate(gtk.Calenda)
  timestamp = to.GPS.timestamp(year = date$year, month = date$month +1, day = date$day, hour = 0 , min = 0, sec = 0)
  file.path = substr(gtkFileChooserGetUri(gtk.path), 8, 500)
  file.path = paste0(file.path,"/", date$year,"-",date$month+1,"-",date$day,".stq")
  id = gtkComboBoxGetActiveText(combo)
  for (s in stations) {
    if (s$name == id)
      id = s$`_id`
  }
  
  gtkTextBufferSetText(tv.buf, "Searching data...")
  data.df = get.data(station.id = id, start.time = timestamp, n =timestamp +60*60*24) 
  gtkTextBufferSetText(tv.buf, "Converting...")
  data = hex2raw(data.df)
  gtkTextBufferSetText(tv.buf, "Writing...")
  stq.f = file(file.path, "wb")
  writeBin(data, stq.f)
  close(stq.f)
  gtkTextBufferSetText(tv.buf, paste("Data pulled at", file.path))
}

gSignalConnect(obj = gtk.button, signal = "clicked", f = pull )

