# ask-MPI

This program retrieves some information about MPI library using MPI calls:

* List of the ranks, hostnames, PIDs and CWDs of all threads.
* The MPI standard version supported by the MPI library.
* The current level of thread support.
* The upper bound for MPI tags.
* The information from `MPI_INFO_ENV`
* The MPI library build information.

When compiled with defined symbol `MPI41` it also tries to decode:

* The info object returned by `MPI_Get_hw_resource_info`.

# Compilation

You can do just

    mpif90 ask_mpi.F90 && ./a.out && rm a.out

or something more elaborate such as adding `-D MPI41` (for the newest MPI
versions) or `-fc=ifx` if it is needed by your Intel's OneAPI environment.
