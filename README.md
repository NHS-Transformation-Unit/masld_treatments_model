<img src="images/TU_logo_large.png" alt="TU logo" width="200" align="right"/>

<br/>


# MASLD Treatment Modelling
This repository contains the code and relevant documentation for the development of the MASLD treatment modelling. This modelling is intended to provide an overview of the populations eligible for each treatment, the anticipated clinical activities and the cost implications of these.


## Table of Contents

- [Repository Structure](#repository-structure)
- [Getting Started](#getting-started)
- [Contributors](#contributors)
- [License](#license)

## Repository Structure

The current structure of the repository is detailed below:

``` plaintext

├───README.md
├───LICENSE
├───.gitignore
├───data
├───documentation
    ├───guidance
    ├───references
    └───specification
├───images
└───src
    └───config
    └───config
    └───config
    
```

- `README.md`: This file containing an overview and instructions for using the repository.
- `LICENSE`: License information for the repository.
- `.gitignore`: Specifies the files and folders that are ignored (not tracked) in the repository.
- `data`: Directory for data files used in the generating assumptions.
- `documentation`: Additional documentation that is helpful for understanding how the model functions and the underlying assumptions.
  - `guidance`: Contains guidance on running the model such as how to run within Posit Cloud.
  - `references`: `.bib` files acting as bibliographies for model development.
  - `specification`: Contains documents and diagrams for developing the model specification.
- `images`: Directory containing any logos and other images used in creating outputs for the repository.
- `src`: All source code used for any analysis and modelling. This is comprised of the following:
  - `config`: Directory contains configuration files for outputs such as css themes.
  - `modules`: Contains the two files for the shiny app. `server.R` for the model logic and `ui.R` for the interface.
  - `requirements`: Contains `packages.R` which contains the model dependencies that need to be installed.

<br/>

## Getting Started
The repository can be cloned locally using:

```
git clone https://github.com/NHS-Transformation-Unit/masld_treatments_model.git
```

<br/>

## Contributors
This repository has been created and developed by:

- [Andy Wilson](https://github.com/ASW-Analyst)

The development of the pathways and reviewing the model has also been supported by:

- [Molly Glynn](https://github.com/MollyGlynn),
- [Martha Holt](https://github.com/marthaholt1),
- [Kira Maguire](https://github.com/kiramaguire)
- Claire Bickerton
- Jason Carrigan
<br/>

## License
This project is licensed under the **GPL-3.0 license** - see the `LICENSE` file for details.