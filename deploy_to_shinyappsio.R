library(rsconnect)
deployApp(appDir='.', appFileManifest = './appFileManifest', appName = 'NextWordPredictor')

# Use the following to grab the last N entries of log entries
rsconnect::showLogs(entries = 50)
