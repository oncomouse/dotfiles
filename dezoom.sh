#!/bin/bash

echo "Disabling automatic video when joining a Zoom meeting..."
defaults write ~/Library/Preferences/us.zoom.config.plist ZDisableVideo 1

for zoom_directory in /Applications/zoom.us.app ~/Applications/zoom.us.app /System/Library/Extensions/ZoomAudioDevice.kext ~/Library/Application\ Support/zoom.us; do
  echo "Checking for Zoom directory at ${zoom_directory}..."
  if [ -d "${zoom_directory}" ]; then
    echo "Removing ${zoom_directory}:"
    rm -rfv "${zoom_directory}"
  else
    echo "${zoom_directory} directory doesn't exist."
  fi
done

for zoom_process in ZoomOpener RingCentralOpener; do
  echo "Checking if ${zoom_process} is running..."
  ZOOM_PID=`pgrep ${zoom_process}`
  if [ $? -eq 0 ]; then
    echo "${zoom_process} running on PID ${ZOOM_PID}, killing process..."
    if kill -9 $ZOOM_PID; then
      echo "Successfully killed ${zoom_process} process."
    else
      echo "Failed to kill ${zoom_process} process on PID ${ZOOM_PID}."
    fi
  else
    echo "${zoom_process} is not running."
  fi
done

for zoom_directory in zoomus ringcentralopener; do
  echo "Checking for ~/.${zoom_directory} directory..."
  if [ -d ~/.${zoom_directory} ]; then
    echo "Removing ~/.${zoom_directory}"
    rm -rfv ~/.${zoom_directory}
  else
    echo "~/.${zoom_directory} directory doesn't exist."
  fi
  if [ ! -e ~/.${zoom_directory} ]; then
    echo "Creating empty ~/.${zoom_directory} file to prevent directory restoration"
    touch ~/.${zoom_directory} && chmod 000 ~/.${zoom_directory}
  fi
done

echo "Done."
