# almanakk-cli

![GitHub Tag](https://img.shields.io/github/v/tag/rtrollebo/almanakk-cli)
![GitHub last commit](https://img.shields.io/github/last-commit/rtrollebo/almanakk-cli)
![Test status](https://github.com/rtrollebo/almanakk-cli/actions/workflows/workflow.yml/badge.svg)

<br/>
<p align="center">
  <img src="assets/almanakk.svg" alt="almanakk log"/>
</p>

This is the almanakk command line program, based on the almanakk-lib library. See [doc](doc/index.html) for library API. 

To build `almanakk-cli` in standard way, the `almanakk-lib` library must be present in `./lib` folder, otherwise build errors will occur. 

    zsh build.zsh

To build `almanakk-cli` with `almanakk-lib` mocked.

    zsh build.zsh mock

The purpose of the mock build is to test application functions, from the github project, without the dependency library present. 

To execute tests, run the standard stack command

    stack test

Static builds of this cli program with the library are available in [rtrollebo.github.io/documentation](https://rtrollebo.github.io/documentation/)

Usage is described in [manual](manual.md). 