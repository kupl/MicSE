(lang dune 2.4.0)

(name micse)

(version 0.0.0)

(generate_opam_files false)

(source
 (uri "git+https://github.com/kupl/MicSE.git"))

(bug_reports "https://github.com/kupl/MicSE/issues")

(homepage "https://github.com/kupl/MicSE")

(license MIT)

(authors "Jisuk Byun <grayplc@korea.ac.kr>" "Heewoong Jang <jormal@korea.ac.kr>")

(maintainers "Jisuk Byun <grayplc@korea.ac.kr>" "Heewoong Jang <jormal@korea.ac.kr>")

(package
 (name micse)
 (synopsis "Michelson Symbolic vErifier, for verifying and testing the integrity of smart contracts in the Tezos blockchain")
 (depends
  (dune :build)
  (michelson :build)
  (tezloa :build)))
