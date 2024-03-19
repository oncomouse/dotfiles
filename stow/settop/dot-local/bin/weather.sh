#!/bin/bash

# Catppuccin Colors
rosewater="#f5e0dc"
flamingo="#f2cdcd"
pink="#f5c2e7"
mauve="#cba6f7"
red="#f38ba8"
maroon="#eba0ac"
peach="#fab387"
yellow="#f9e2af"
green="#a6e3a1"
teal="#94e2d5"
sky="#89dceb"
sapphire="#74c7ec"
blue="#89b4fa"
lavender="#b4befe"
text="#cdd6f4"
subtext1="#bac2de"
subtext0="#a6adc8"
overlay2="#9399b2"
overlay1="#7f849c"
overlay0="#6c7086"
surface2="#585b70"
surface1="#45475a"
surface0="#313244"
base="#1e1e2e"
mantle="#181825"
crust="#11111b"

## Collect data
cache_dir="$HOME/.cache/eww/weather"
mkdir -p "$cache_dir"
cache_weather_stat=${cache_dir}/weather-stat
cache_weather_degree=${cache_dir}/weather-degree
cache_weather_quote=${cache_dir}/weather-quote
cache_weather_hex=${cache_dir}/weather-hex
cache_weather_icon=${cache_dir}/weather-icon

## Weather data
KEY="$(bash ~/.local/bin/weather-secret.sh)"
ID="4682464"
UNIT="imperial"	# Available options : 'metric' or 'imperial'

## Make cache dir
if [[ ! -d "$cache_dir" ]]; then
	mkdir -p ${cache_dir}
fi

## Get data
get_weather_data() {
	weather=`curl -sf "http://api.openweathermap.org/data/2.5/weather?APPID="$KEY"&id="$ID"&units="$UNIT""`
	echo "http://api.openweathermap.org/data/2.5/weather?APPID="$KEY"&id="$ID"&units="$UNIT
	echo ${weather}

	if [ ! -z "$weather" ]; then
		weather_temp=`echo "$weather" | jq ".main.temp" | cut -d "." -f 1`
		weather_icon_code=`echo "$weather" | jq -r ".weather[].icon" | head -1`
		weather_description=`echo "$weather" | jq -r ".weather[].description" | head -1 | sed -e "s/\b\(.\)/\u\1/g"`

		#Big long if statement of doom
		if [ "$weather_icon_code" == "50d"  ]; then
			weather_icon=" "
			weather_quote="Forecast says it's misty \nMake sure you don't get lost on your way..."
			weather_hex="$sapphire"
		elif [ "$weather_icon_code" == "50n"  ]; then
			weather_icon=" "
			weather_quote="Forecast says it's a misty night \nDon't go anywhere tonight or you might get lost..."
			weather_hex="$sapphire"
		elif [ "$weather_icon_code" == "01d"  ]; then
			weather_icon=" "
			weather_quote="It's a sunny day, gonna be fun! \nDon't go wandering all by yourself though..."
			weather_hex="$yellow"
		elif [ "$weather_icon_code" == "01n"  ]; then
			weather_icon=" "
			weather_quote="It's a clear night \nYou might want to take a evening stroll to relax..."
			weather_hex="$flamingo"
		elif [ "$weather_icon_code" == "02d"  ]; then
			weather_icon=" "
			weather_quote="It's  cloudy, sort of gloomy \nYou'd better get a book to read..."
			weather_hex="$mauve"
		elif [ "$weather_icon_code" == "02n"  ]; then
			weather_icon=" "
			weather_quote="It's a cloudy night \nHow about some hot chocolate and a warm bed?"
			weather_hex="$mauve"
		elif [ "$weather_icon_code" == "03d"  ]; then
			weather_icon=" "
			weather_quote="It's  cloudy, sort of gloomy \nYou'd better get a book to read..."
			weather_hex="$mauve"
		elif [ "$weather_icon_code" == "03n"  ]; then
			weather_icon=" "
			weather_quote="It's a cloudy night \nHow about some hot chocolate and a warm bed?"
			weather_hex="$mauve"
		elif [ "$weather_icon_code" == "04d"  ]; then
			weather_icon=" "
			weather_quote="It's  cloudy, sort of gloomy \nYou'd better get a book to read..."
			weather_hex="$mauve"
		elif [ "$weather_icon_code" == "04n"  ]; then
			weather_icon=" "
			weather_quote="It's a cloudy night \nHow about some hot chocolate and a warm bed?"
			weather_hex="$mauve"
		elif [ "$weather_icon_code" == "09d"  ]; then
			weather_icon=" "
			weather_quote="It's rainy, it's a great day! \nGet some ramen and watch as the rain falls..."
			weather_hex="$blue"
		elif [ "$weather_icon_code" == "09n"  ]; then
			weather_icon=" "
			weather_quote=" It's gonna rain tonight it seems \nMake sure your clothes aren't still outside..."
			weather_hex="$blue"
		elif [ "$weather_icon_code" == "10d"  ]; then
			weather_icon=" "
			weather_quote="It's rainy, it's a great day! \nGet some ramen and watch as the rain falls..."
			weather_hex="$blue"
		elif [ "$weather_icon_code" == "10n"  ]; then
			weather_icon=" "
			weather_quote=" It's gonna rain tonight it seems \nMake sure your clothes aren't still outside..."
			weather_hex="$blue"
		elif [ "$weather_icon_code" == "11d"  ]; then
			weather_icon=""
			weather_quote="There's storm for forecast today \nMake sure you don't get blown away..."
			weather_hex="$peach"
		elif [ "$weather_icon_code" == "11n"  ]; then
			weather_icon=""
			weather_quote="There's gonna be storms tonight \nMake sure you're warm in bed and the windows are shut..."
			weather_hex="$peach"
		elif [ "$weather_icon_code" == "13d"  ]; then
			weather_icon=" "
			weather_quote="It's gonna snow today \nYou'd better wear thick clothes and make a snowman as well!"
			weather_hex="$text"
		elif [ "$weather_icon_code" == "13n"  ]; then
			weather_icon=" "
			weather_quote="It's gonna snow tonight \nMake sure you get up early tomorrow to see the sights..."
			weather_hex="$text"
		elif [ "$weather_icon_code" == "40d"  ]; then
			weather_icon=" "
			weather_quote="Forecast says it's misty \nMake sure you don't get lost on your way..."
			weather_hex="$sapphire"
		elif [ "$weather_icon_code" == "40n"  ]; then
			weather_icon=" "
			weather_quote="Forecast says it's a misty night \nDon't go anywhere tonight or you might get lost..."
			weather_hex="$sapphire"
		else 
			weather_icon=" "
			weather_quote="Sort of odd, I don't know what to forecast \nMake sure you have a good time!"
			weather_hex="$mauve"
		fi
		echo "$weather_icon" >  ${cache_weather_icon}
		echo "$weather_description" > ${cache_weather_stat}
		echo "$weather_temp""°F" > ${cache_weather_degree}
		echo -e "$weather_quote" > ${cache_weather_quote}
		echo "$weather_hex" > ${cache_weather_hex}
	else
		echo "Weather Unavailable" > ${cache_weather_stat}
		echo " " > ${cache_weather_icon}
		echo -e "Ah well, no weather huh? \nEven if there's no weather, it's gonna be a great day!" > ${cache_weather_quote}
		echo "-" > ${cache_weather_degree}
		echo "$mauve" > ${cache_weather_hex}
	fi
}

## Execute
if [[ "$1" == "--getdata" ]]; then
	get_weather_data
elif [[ "$1" == "--icon" ]]; then
	cat ${cache_weather_icon}
elif [[ "$1" == "--temp" ]]; then
	cat ${cache_weather_degree}
elif [[ "$1" == "--hex" ]]; then
	cat ${cache_weather_hex}
elif [[ "$1" == "--stat" ]]; then
	cat ${cache_weather_stat}
elif [[ "$1" == "--quote" ]]; then
	cat ${cache_weather_quote} | head -n1
elif [[ "$1" == "--quote2" ]]; then
	cat ${cache_weather_quote} | tail -n1
fi
