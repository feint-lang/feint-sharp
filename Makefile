packages := ~/.nuget/packages

PHONY := build
build:
	@dotnet build

PHONY := run
run:
	@dotnet run

PHONY := test
test:
	@dotnet test --logger:"console;verbosity=detailed"

PHONY := add-fantomas
add-fantomas:
	test -f .config/dotnet-tools.json || dotnet new tool-manifest
	dotnet tool install fantomas

PHONY := fmt
fmt:
	@dotnet tool run fantomas . -- --recurse
