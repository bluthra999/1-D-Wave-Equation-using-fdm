# 1D Wave Equation Solver - Standing Waves & Pulse Propagation

[![Fortran](https://img.shields.io/badge/Fortran-2018-%23734F96?logo=fortran)](https://fortran-lang.org)
[![GNUplot](https://img.shields.io/badge/GNUplot-5.4-%23F0A33F)](http://www.gnuplot.info/)

Numerical solutions for the 1D wave equation modeling:
- Standing wave eigenmodes on a fixed string
- Gaussian pulse propagation with boundary reflections

![Standing Wave Demo](docs/standing_wave.gif) ![Pulse Propagation Demo](docs/wave_pulse.gif)

## Features

### Standing Wave Simulation
- Solves $$u_{tt} = c^2u_{xx}$$ with Dirichlet boundary conditions
- Finite-Difference Time-Domain (FDTD) method implementation
- Standing wave initialization with harmonic modes
- Automatic animation generation using GNUplot

### Wave Pulse Simulation
- Models Gaussian pulse propagation: $$u(x,0) = e^{-(x-x_0)^2/\sigma^2}$$
- Implements absorbing/reflecting boundary conditions
- Staggered leapfrog scheme (2nd order in space and time)
- CFL condition enforcement for numerical stability

## Installation

### Requirements
- Fortran compiler (gfortran recommended)
- GNUplot 5.4+ for visualization

