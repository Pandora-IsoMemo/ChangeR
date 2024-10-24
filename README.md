# ChangeR

An App to display and identify isotopic change points in human tooth sections using a ChangeR algorithm (v 0.01) that is based on MCP.

## Release notes:

- see `NEWS.md`

## Notes for developers

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is build automatically via github action.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```
