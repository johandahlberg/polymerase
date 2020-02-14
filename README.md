# Polymerase

There is a growing interest in storing digital data as DNA. DNA is interesting as a
storage medium since not only is it a very dense way of storing data, but it is
also relatively stable and easy to copy. These properties make it an intersting
candidate for long term storage of data.

# Running Polymerase

# TODOs

Here is an incomplete list of things that I aim to do with Polymerase. If you are
interested in contributing to the project, this can be a good place to start looking.

**Framework**

- Write an extendable codec trait, to make it easy to create new encoders from
  existing methods.

**Encoding/decoding schemes**

- Implement block encoding/decoding
- Implement Reed-Solomon Error corrections codes, with inner and outer-blocks like in Organick et al. (2017)
- Implement DNA Fountain strategy from Erlich and Zielinski (2017)
- Implement base rotation scheme [REF]

**Utilities**

- Simulator to add errors and drop outs to sequencing reads

# References

Anavy, L., Vaknin, I., Atar, O., Amit, R., & Yakhini, Z. (2019). Data storage in DNA with fewer synthesis cycles using composite DNA letters. Nature Biotechnology. https://doi.org/10.1038/s41587-019-0240-x

Ceze, L., Nivala, J., & Strauss, K. (2019). Molecular digital data storage using DNA. Nature Reviews. Genetics, 20(8), 456–466.

Erlich, Y., & Zielinski, D. (2017). DNA Fountain enables a robust and efficient storage architecture. Science, 355(6328), 950–954.

Organick, L., Ang, S. D., Chen, Y.-J., Lopez, R., Yekhanin, S., Makarychev, K., … Strauss, K. (2018). Random access in large-scale DNA data storage. Nature Biotechnology, 36(3), 242–248.
