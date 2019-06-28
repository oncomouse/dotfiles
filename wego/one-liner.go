package frontends

import (
    "fmt"
    "log"

    colorable "github.com/mattn/go-colorable"
    runewidth "github.com/mattn/go-runewidth"
    "github.com/schachmat/wego/iface"
)

type oneLinerConfig struct {
    unit iface.UnitSystem
}
func (c *oneLinerConfig) formatWind(cond iface.Cond) string {
    windDir := func(deg *int) string {
        if deg == nil {
            return "?"
        }
        arrows := []string{"↓", "↙", "←", "↖", "↑", "↗", "→", "↘"}
        return "" + arrows[((*deg+22)%360)/45]
    }
    color := func(spdKmph float32) string {
        // colmap := []struct {
        //     maxtemp float32
        //     color   int
        // }{
        //     {0, 46}, {4, 82}, {7, 118}, {10, 154}, {13, 190},
        //     {16, 226}, {20, 220}, {24, 214}, {28, 208}, {32, 202},
        // }

        // col := 196
        // for _, candidate := range colmap {
        //     if spdKmph < candidate.maxtemp {
        //         col = candidate.color
        //         break
        //     }
        // }

        s, _ := c.unit.Speed(spdKmph)
        // return fmt.Sprintf("\033[38;5;%03dm%d\033[0m", col, int(s))
        return fmt.Sprintf("%d", int(s))
    }

    // _, u := c.unit.Speed(0.0)

    if cond.WindspeedKmph == nil {
        return windDir(cond.WinddirDegree)
    }
    s := *cond.WindspeedKmph

    if cond.WindGustKmph != nil {
        if g := *cond.WindGustKmph; g > s {
            return fmt.Sprintf("%s%s–%s", windDir(cond.WinddirDegree), color(s), color(g))
        }
    }

    return fmt.Sprintf("%s%s", windDir(cond.WinddirDegree), color(s))//, u)
}

func (c *oneLinerConfig) formatTemp(cond iface.Cond) string {
    color := func(temp float32) string {
        // colmap := []struct {
        //     maxtemp float32
        //     color   int
        // }{
        //     {-15, 21}, {-12, 27}, {-9, 33}, {-6, 39}, {-3, 45},
        //     {0, 51}, {2, 50}, {4, 49}, {6, 48}, {8, 47},
        //     {10, 46}, {13, 82}, {16, 118}, {19, 154}, {22, 190},
        //     {25, 226}, {28, 220}, {31, 214}, {34, 208}, {37, 202},
        // }

        // col := 196
        // for _, candidate := range colmap {
        //     if temp < candidate.maxtemp {
        //         col = candidate.color
        //         break
        //     }
        // }
        t, _ := c.unit.Temp(temp)
        // return fmt.Sprintf("\033[38;5;%03dm%d\033[0m", col, int(t))
        return fmt.Sprintf("%d", int(t))
    }

    _, u := c.unit.Temp(0.0)

    if cond.TempC == nil {
        return fmt.Sprintf("?%s", u)
    }

    t := *cond.TempC
    if cond.FeelsLikeC != nil {
        fl := *cond.FeelsLikeC
        return fmt.Sprintf("%s(%s)%s", color(t), color(fl), u)
    }
    return fmt.Sprintf("%s%s", color(t), u)
}

func (c *oneLinerConfig) formatCond(cur []string, cond iface.Cond, current bool) (ret []string) {
    codes := map[iface.WeatherCode]string{
        iface.CodeUnknown:             "✨",
        iface.CodeCloudy:              "☁️",
        iface.CodeFog:                 "🌫",
        iface.CodeHeavyRain:           "🌧",
        iface.CodeHeavyShowers:        "🌧",
        iface.CodeHeavySnow:           "❄️",
        iface.CodeHeavySnowShowers:    "❄️",
        iface.CodeLightRain:           "🌦",
        iface.CodeLightShowers:        "🌦",
        iface.CodeLightSleet:          "🌧",
        iface.CodeLightSleetShowers:   "🌧",
        iface.CodeLightSnow:           "🌨",
        iface.CodeLightSnowShowers:    "🌨",
        iface.CodePartlyCloudy:        "⛅️",
        iface.CodeSunny:               "☀️",
        iface.CodeThunderyHeavyRain:   "🌩",
        iface.CodeThunderyShowers:     "⛈",
        iface.CodeThunderySnowShowers: "⛈",
        iface.CodeVeryCloudy:          "☁️",
    }

    icon, ok := codes[cond.Code]
    if !ok {
        log.Fatalln("emoji-frontend: The following weather code has no icon:", cond.Code)
    }
    if runewidth.StringWidth(icon) == 1 {
        icon += " "
    }

    desc := cond.Desc
    if !current {
        desc = runewidth.Truncate(runewidth.FillRight(desc, 13), 13, "…")
    }

    ret = append(ret, fmt.Sprintf("%v %v%v %v%v",icon , "🌡️", c.formatTemp(cond), "🌬️", c.formatWind(cond)))
    return
}

func (c *oneLinerConfig) Setup() {
}

func (c *oneLinerConfig) Render(r iface.Data, unitSystem iface.UnitSystem) {
    c.unit = unitSystem

    stdout := colorable.NewColorableStdout()

    out := c.formatCond(make([]string, 5), r.Current, true)
    for _, val := range out {
        fmt.Fprintf(stdout, val)
    }
}

func init() {
    iface.AllFrontends["one-liner"] = &oneLinerConfig{}
}
