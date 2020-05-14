# Yarn is included in the node image.
FROM node AS build

COPY . /sources
WORKDIR /sources
RUN yarn install
# We're required to build the app at least once to download the dependencies.
RUN yarn elm make src/Main.elm --output=temp.html && rm temp.html
RUN yarn build

# Deployment is done via the PHP apache image.
FROM php:7.2-apache AS deploy

COPY --from=build /sources/dist/ /var/www/html/