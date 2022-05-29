return {
	s(
		"html5",
		fmt(
			[[<!DOCTYPE html>
<html lang="{}">
	<head>
		<title>{}</title>
		<meta charset="UTF-8">
		<meta name="viewport" content="width=device-width, initial-scale=1">
		<link href="{}" rel="stylesheet">
	</head>
	<body>
	{}
	</body>
</html>]],
			{
				i(1, "en"),
				i(2),
				i(3, "css/style.css"),
				i(4),
			}
		)
	),
}
