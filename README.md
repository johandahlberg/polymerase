# Disclaimer

Part of the algorithm(s) here are covered by [US patent 16/032,946](https://patents.google.com/patent/US20190020353A1/en).

# Polymerase

I have been following the the rise of [DNA as a storage medium for digital data](https://en.wikipedia.org/wiki/DNA_digital_data_storage) with great interest. DNA is interesting as a storage medium since not only is it a very
dense way of storing data, but it is also relatively stable and easy to copy. These properties make it an intersting
candidate for long term storage of data.

This idea behind this repo has been to experiment with DNA storage codecs, and try to separate the different
components from each other so that new encoding schemas can easily be constructed by combining different components.
In addition to this I have aimed to re-implement codec strategies described in various scientific publications.

The focus of the code is on readability and clear separation of concerns rather than on performance.

Please note! This project is pre-alpha, and concidering that it has been a written to satisfy my personal curiosity,
it may never leave that stage.

# Running Polymerase

To get an executable of Polymerase install [sbt](https://www.scala-sbt.org/), and run:

```
sbt universal:packageBin
```

You will now find a zip file under: `target/universal`. If you unzip this file you will find the relevant
run scripts under `/bin` in that directory.

The different polymerase executable variants, all read data from `stdin`, and can be run for example like this.

```
cat [my super cool file] | polymerase-encode | polymerase-decode > [old file looking like new]
```

You should be able to verify that the file you just wrote is identical, with for example `md5sum`.

## Guide to the relevant executables

There are a number of diffrent executables available in the bin dir. They all do slightly different things:

- `polymerase-encode/polymerase-decode`: Encode and decode digital data to DNA without any error correction
- `polymerase-rs-encode/polymerase-rs-decode`: Encode and decode data to DNA with [Reed-Solomon error correction](https://en.wikipedia.org/wiki/Reed%E2%80%93Solomon_error_correction)
- `polymerase-fountain-encode/polymerase-fountain-decode`: Encode and decode data to DNA with using [LT-codes](https://en.wikipedia.org/wiki/Luby_transform_code) for erasure coding
- `polymerase-drop-reads`: Utility which randomly drops reads (for testing erasure coding)
- `polymerase-simulate-errors`: Utility for adding base-swap errors (for testing error detection/correction)

# TODOs

Here is an incomplete list of things that I aim to do with Polymerase. If you are interested in
contributing to the project, this can be a good place to start looking.

- Harmonize the interfaces for all codecs to use `Package` as input/output.
- Look into implementing DNA Fountain strategy from Erlich and Zielinski (2017), including combining, RS-encoding and,
  filtering created reads for e.g. homopolymers

# An incomplete list of relevant references data storage in DNA

- Anavy, L., Vaknin, I., Atar, O., Amit, R., & Yakhini, Z. (2019). Data storage in DNA with fewer synthesis cycles using composite DNA letters. Nature Biotechnology. https://doi.org/10.1038/s41587-019-0240-x
- Ceze, L., Nivala, J., & Strauss, K. (2019). Molecular digital data storage using DNA. Nature Reviews. Genetics, 20(8), 456–466.
- Erlich, Y., & Zielinski, D. (2017). DNA Fountain enables a robust and efficient storage architecture. Science, 355(6328), 950–954.
- Organick, L., Ang, S. D., Chen, Y.-J., Lopez, R., Yekhanin, S., Makarychev, K., … Strauss, K. (2018). Random access in large-scale DNA data storage. Nature Biotechnology, 36(3), 242–248.
