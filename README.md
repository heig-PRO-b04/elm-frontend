# heig-PRO-b04/elm-frontend

[![Build Status](https://travis-ci.com/heig-PRO-b04/elm-frontend.svg?branch=master)](https://travis-ci.com/heig-PRO-b04/elm-frontend)

An application to create, edit and delete polls that can be answered in real
time. In this repository, you'll find the codebase of the web client of the
system.

This software is developed as semester project (PRO) at HEIG-VD, academic year
2019/20.

## Development team:

| Name                                   |
|----------------------------------------|
| Matthieu Burguburu (ass. project lead) |
| David Dupraz                           |
| Clarisse Fleurimont                    |
| Alexandre Piveteau (project lead)      |
| Guy-Laurent Subri                      |

## Dependencies

This project uses [yarn](https://yarnpkg.com). Make sure you have at least the
version 2.x on your build machine.

## Building and installing

To launch the app locally, you need to :

1. Clone the repository
2. Run `yarn install` at the root of the repo. This will resolve all the
   required dependencies.
3. Run `yarn dev` and go to the port indicated in the command line.

To build the production assets of the application, you need to :

1. Clone the repository
2. Run `yarn install` at the root of the repo. This will resolve all the
   required dependencies.
3. Run `yarn build` and go to the `dist/` folder.