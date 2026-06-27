
// fx_GetMultipleWeatherData
// use latitude and longitude to get weather data
// return data based on parameters
//    Latitude  = latitude, decimal
//    Longitude = longitude, decimal
//    ForecastDays = forward days, integer
//    HistoryDays = backwards days, integer
//    DailyRequest = daily data request, comma separated list
//    HourlyRequest = hourly data request, comma separated list
//    CurrentRequest = current data request, comma separated list
// see list at https://open-meteo.com/en/docs


(lats as list, lons as list) =>
let
    Latitude  = Text.Combine(List.Transform(lats, Text.From), ","),
    Longitude = Text.Combine(List.Transform(lons, Text.From), ","),
    ForecastDays = Text.From(p_ForecastDays),
    HistoryDays = Text.From(p_HistoryDays),
    DailyRequest = Text.From(p_DailyRequest),
    HourlyRequest = Text.From(p_HourlyRequest),
    CurrentRequest = Text.From(p_CurrentRequest),

    Source = Json.Document(
        Web.Contents(
            "https://api.open-meteo.com/v1/forecast",
            [
                Query = [
                    latitude = Latitude,
                    longitude = Longitude,
                    daily = DailyRequest,
                    hourly = HourlyRequest,
                    current = CurrentRequest,
                    timezone = "auto",
                    forecast_days = ForecastDays,
                    past_days = HistoryDays
                ],
                Headers = [
                    #"Accept-Encoding" = "identity"
                ]
            ]
        )
    ),
    #"Converted to Table" = Table.FromRecords(Source)
in
    #"Converted to Table"

// Parameters
p_CurrentRequest temperature_2m,relative_humidity_2m,apparent_temperature,precipitation,weather_code,wind_speed_10m,is_day
p_DailyRequest weather_code,temperature_2m_max,temperature_2m_min,sunrise,sunset,precipitation_sum,precipitation_probability_max,uv_index_max
p_HourlyRequest temperature_2m,relative_humidity_2m,precipitation,precipitation_probability,visibility,weather_code,cloud_cover,surface_pressure

// WeatherCurrent
// dynamically expand current weather
// dynamically change type based on columns returned
let
    Source = City,

    // Keep only required columns
    #"Removed Other Columns" = Table.SelectColumns(Source,{"city_id", "current"}),
    
    Cols = Record.FieldNames(#"Removed Other Columns"{0}[current]),
    NewCols = List.Transform(Cols, each "current." & _),
    
    // Expand
    #"Expanded current" = Table.ExpandRecordColumn(
        #"Removed Other Columns",
        "current",
        Cols,
        NewCols
    ),

    TypePairs = List.Transform(NewCols, each
        {_,
            if Text.EndsWith(_, ".time") then type datetime
            else if Text.EndsWith(_, ".sunrise") or Text.EndsWith(_, ".sunset") then type datetime
            else if Text.EndsWith(_, ".weather_code") then Int64.Type
            else if Text.Contains(_, "probability") then Percentage.Type
            else type number
        }),
    #"Changed Type" = Table.TransformColumnTypes(#"Expanded current", TypePairs)
in
    #"Changed Type"


// WeatherHourly
// dynamically expand hourly weather
// dynamically change type based on columns returned
// add additional helper columns for dates
let
    Source = City,

    // Keep only required columns
    #"Removed Other Columns" = Table.SelectColumns(Source,{"city_id", "utc_offset_seconds", "hourly"}),

    // Convert column into a nested table
    #"Transformed Column" = Table.TransformColumns(
        #"Removed Other Columns",
        {
            {
                "hourly",
                each Table.FromColumns(
                    Record.FieldValues(_),
                    Record.FieldNames(_)
                )
            }
        }
    ),

    Cols = Table.ColumnNames(#"Transformed Column"{0}[hourly]),
    NewCols = List.Transform(Cols, each "hourly." & _),
    // Expand
    #"Expanded hourly" = Table.ExpandTableColumn(
        #"Transformed Column",
        "hourly",
        Cols,
        NewCols
    ),

    TypePairs = List.Transform(NewCols, each
        {_,
            if Text.EndsWith(_, ".time") then type datetime
            else if Text.EndsWith(_, ".sunrise") or Text.EndsWith(_, ".sunset") then type datetime
            else if Text.EndsWith(_, ".weather_code") then Int64.Type
            else if Text.Contains(_, "probability") then Percentage.Type
            else type number
        }),

    TransformPairs = List.Transform(NewCols, each
        {_,
            if Text.Contains(_, "probability") then (x) => x / 100
            else (x) => x
        }),

    // Convert percentages
    #"Transformed Columns" = Table.TransformColumns(#"Expanded hourly", TransformPairs),
    
    #"Changed Type" = Table.TransformColumnTypes(#"Transformed Columns", TypePairs),

    // Add date columns
    #"Added hourly.date" = Table.AddColumn(#"Changed Type", "hourly.date", each Date.From([hourly.time]), type date),
    #"Added UTC" = Table.AddColumn(#"Added hourly.date", "hourly.datetimeUTC", each [hourly.time] - #duration(0, 0, 0, [utc_offset_seconds])),
    #"Renamed Columns" = Table.RenameColumns(#"Added UTC",{{"hourly.time", "hourly.datetime"}})
in
    #"Renamed Columns"

// WeatherDaily
// dynamically expand daily weather
// dynamically change type based on columns returned
let
    Source = City,

    // Keep only required columns
    #"Removed Other Columns" = Table.SelectColumns(Source,{"city_id", "daily"}),

    // Convert column into a nested table
    #"Transformed Column" = Table.TransformColumns(
        #"Removed Other Columns",
        {
            {
                "daily",
                each Table.FromColumns(
                    Record.FieldValues(_),
                    Record.FieldNames(_)
                )
            }
        }
    ),

    Cols = Table.ColumnNames(#"Transformed Column"{0}[daily]),
    NewCols = List.Transform(Cols, each "daily." & _),
    // Expand
    #"Expanded daily" = Table.ExpandTableColumn(
        #"Transformed Column",
        "daily",
        Cols,
        NewCols
    ),

    TypePairs = List.Transform(NewCols, each
        {_,
            if Text.EndsWith(_, ".time") then type date
            else if Text.EndsWith(_, ".sunrise") or Text.EndsWith(_, ".sunset") then type datetime
            else if Text.EndsWith(_, ".weather_code") then Int64.Type
            else if Text.Contains(_, "probability") then Percentage.Type
            else type number
        }),

    TransformPairs = List.Transform(NewCols, each
        {_,
            if Text.Contains(_, "probability") then (x) => x / 100
            else (x) => x
        }),

    // Convert percentages  
    #"Transformed Columns" = Table.TransformColumns(#"Expanded daily", TransformPairs),

    #"Changed Type" = Table.TransformColumnTypes(#"Transformed Columns", TypePairs),

    // Cleanup for consistency
    #"Renamed Columns" = Table.RenameColumns(#"Changed Type",{{"daily.time", "daily.date"}})
in
    #"Renamed Columns"

// WeatherCodes
// define svg paths for WMO weather codes
let
    g_clear   = "<circle cx='16' cy='16' r='6'/><path d='M16 3v3M16 26v3M3 16h3M26 16h3M6.5 6.5l2.1 2.1M23.4 23.4l2.1 2.1M6.5 25.5l2.1-2.1M23.4 8.6l2.1-2.1'/>",
    g_partly  = "<circle cx='12' cy='11' r='4.5'/><path d='M12 3v2M5 11H3M6.3 5.3l1.4 1.4M17.7 5.3l-1.4 1.4'/><path d='M11 24a4.5 4.5 0 0 1-.6-8.96A6 6 0 0 1 22 16.5 4 4 0 0 1 21.5 24z'/>",
    g_cloudy  = "<path d='M9 23a5.5 5.5 0 0 1-.7-10.96A7 7 0 0 1 22 14a4.5 4.5 0 0 1-.5 9z'/>",
    g_fog     = "<path d='M9 18a5 5 0 0 1-.6-9.96A6.5 6.5 0 0 1 21 11a4 4 0 0 1 .3 7M6 23h20M9 27h14'/>",
    g_drizzle = "<path d='M9 19a5 5 0 0 1-.6-9.96A6.5 6.5 0 0 1 21 12a4 4 0 0 1 .3 7'/><path d='M11 24v2M16 24v3M21 24v2'/>",
    g_rain    = "<path d='M9 18a5 5 0 0 1-.6-9.96A6.5 6.5 0 0 1 21 11a4 4 0 0 1 .3 7'/><path d='M10 23l-1.5 4M16 23l-1.5 4M22 23l-1.5 4'/>",
    g_showers = "<path d='M11 6a4 4 0 0 1 3.4 6'/><path d='M9 19a5 5 0 0 1-.6-9.96A6.5 6.5 0 0 1 21 12a4 4 0 0 1 .3 7'/><path d='M10 23l-1.5 4M16 23l-1.5 4M22 23l-1.5 4'/>",
    g_snow    = "<path d='M9 17a5 5 0 0 1-.6-9.96A6.5 6.5 0 0 1 21 10a4 4 0 0 1 .3 7'/><path d='M11 22v3M9.5 23.5h3M16 25v3M14.5 26.5h3M21 22v3M19.5 23.5h3'/>",
    g_thunder = "<path d='M9 17a5 5 0 0 1-.6-9.96A6.5 6.5 0 0 1 21 10a4 4 0 0 1 .3 7'/><path d='M15 19l-3 5h4l-3 5'/>",

    rows = {
        {0,"Clear sky",g_clear}, {1,"Mainly clear",g_partly}, {2,"Partly cloudy",g_partly},
        {3,"Overcast",g_cloudy}, {45,"Fog",g_fog}, {48,"Rime fog",g_fog},
        {51,"Light drizzle",g_drizzle}, {53,"Drizzle",g_drizzle}, {55,"Dense drizzle",g_drizzle},
        {56,"Freezing drizzle",g_drizzle}, {57,"Freezing drizzle",g_drizzle},
        {61,"Slight rain",g_rain}, {63,"Rain",g_rain}, {65,"Heavy rain",g_rain},
        {66,"Freezing rain",g_rain}, {67,"Freezing rain",g_rain},
        {71,"Slight snow",g_snow}, {73,"Snow",g_snow}, {75,"Heavy snow",g_snow}, {77,"Snow grains",g_snow},
        {80,"Slight showers",g_showers}, {81,"Showers",g_showers}, {82,"Violent showers",g_showers},
        {85,"Snow showers",g_snow}, {86,"Heavy snow showers",g_snow},
        {95,"Thunderstorm",g_thunder}, {96,"Thunderstorm with hail",g_thunder}, {99,"Thunderstorm with hail",g_thunder}
    },
    WeatherIcons = #table(
        type table [weather_code = Int64.Type, description = text, svg_path = text],
        rows
    )
in
    WeatherIcons

// Icons
// define svg paths for icons
let
    pin      = "<path d='M20 10c0 5-8 11-8 11s-8-6-8-11a8 8 0 0 1 16 0z'/><circle cx='12' cy='10' r='2.5'/>",
    wind     = "<path d='M3 8h11a3 3 0 1 0-3-3M3 12h15a3 3 0 1 1-3 3M3 16h9a2.5 2.5 0 1 1-2.5 2.5'/>",
    humidity = "<path d='M12 3s6 6.5 6 11a6 6 0 0 1-12 0c0-4.5 6-11 6-11z'/>",
    rain     = "<path d='M12 3v2M3.5 12a8.5 8.5 0 0 1 17 0zM12 12v7a2.5 2.5 0 0 1-5 0'/>",
    temp     = "<path d='M14 14.8V5a2 2 0 0 0-4 0v9.8a4 4 0 1 0 4 0z'/>",
    sunrise  = "<path d='M12 3v5M8 7l4-4 4 4M3 18h2M19 18h2M5.5 13.5l1.4 1.4M17.1 14.9l1.4-1.4M2 22h20M7 18a5 5 0 0 1 10 0'/>",
    sunset   = "<path d='M12 8V3M8 5l4 3 4-3M3 18h2M19 18h2M5.5 13.5l1.4 1.4M17.1 14.9l1.4-1.4M2 22h20M7 18a5 5 0 0 1 10 0'/>",
    sun      = "<circle cx='12' cy='12' r='4.5'/><path d='M12 2v2M12 20v2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M2 12h2M20 12h2M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4'/>",
    moon     = "<path d='M21 12.8A9 9 0 1 1 11.2 3a7 7 0 0 0 9.8 9.8z'/>",
    uv         = "<circle cx='12' cy='10' r='3.6'/><path d='M12 4.4V2.8M5 10H3.4M20.6 10H19M6.6 4.6l1.1 1.1M17.4 4.6l-1.1 1.1M6.6 15.4l1.1-1.1M17.4 15.4l-1.1-1.1M3 21h18'/>",
    visibility = "<path d='M2 12s3.6-6.5 10-6.5 10 6.5 10 6.5-3.6 6.5-10 6.5S2 12 2 12z'/><circle cx='12' cy='12' r='2.8'/>",
    pressure   = "<circle cx='12' cy='12' r='8.5'/><path d='M12 12l4-2.6M12 3.6v1.6'/><circle cx='12' cy='12' r='1'/>",
    airquality = "<path d='M3 9c2-2.2 5-2.2 7 0s5 2.2 7 0'/><path d='M3 15c2-2.2 5-2.2 7 0s5 2.2 7 0'/><circle cx='6.5' cy='12' r='.8'/><circle cx='12' cy='12.4' r='.8'/><circle cx='17.5' cy='12' r='.8'/>",

    rows = {
        {"pin",pin},{"wind",wind},{"humidity",humidity},{"rain",rain},{"temp",temp},
        {"sunrise",sunrise},{"sunset",sunset},{"sun",sun},{"moon",moon},{"uv",uv},
        {"visibility",visibility},{"pressure",pressure},{"airquality",airquality}
        },
    UIIcons = #table( type table [icon = text, svg_path = text], rows )
in
    UIIcons