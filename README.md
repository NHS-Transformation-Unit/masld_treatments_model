<img src="images/TU_logo_large.png" alt="TU logo" width="200" align="right"/>

# MALSD Treatment Modelling
This repository contains the code and relevant documentation for the development of the MASLD treatment modelling. This modelling is intended to provide an overview of the populations eligible for each treatment, the anticipated clinical activities and the cost implications of these.

**This repository is still being developed so the structure and contents will change.**

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
    ├───references
    └───specification
├───images
└───src
    └───config
    
```

- `README.md`: This file containing an overview and instructions for using the repository.
- `LICENSE`: License information for the repository.
- `.gitignore`: Specifies the files and folders that are ignored (not tracked) in the repository.
- `data`: Directory for data files used in the generating assumptions.
- `documentation`: Additional documentation that is helpful for understanding how the model functions and the underlying assumptions.
  - `references`: `.bib` files acting as bibliographies for model development.
  - `specification`: Contains documents and diagrams for developing the model specification.
- `images`: Directory containing any logos and other images used in creating outputs for the repository.
- `src`: All source code used for any analysis and modelling. This is comprised of the following:
  - `config`: Directory contains configuration files for outputs such as css themes.

<br/>

## Getting Started
The repository can be cloned locally using:

```
git clone https://github.com/NHS-Transformation-Unit/HSDS_MASH.git
```

<br/>

## Contributors
This repository has been created and developed by:

- [Andy Wilson](https://github.com/ASW-Analyst)

<br/>

## License
This project is licensed under the **GPL-3.0 license** - see the `LICENSE` file for details.