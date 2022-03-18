IMAGENAME=karttasovellus
docker build -t $IMAGENAME .
# testaa ensin interaktiivisesti R:ssä esim. ajamalla < R -e "shiny::runApp()" >
docker run -ti --rm karttasovellus bash
docker run --env-file .Renviron $IMAGENAME