# To do

Found in solanaceae_ngc_ft1 (1f4e8d7) while porting it. To tell Green-Sky.

- CUBIC: constant 0.4 is per packet but windows are in bytes, so cold windows
  grow ~1000x too slowly. We scale it by the segment size (`Tox.Share.Congestion`).
- `FlowOnly::canSend`/`CUBIC::canSend`: the dither compares an unsigned
  random number with a negative difference, so it fires whenever there's a
  remainder: the budget always rounds up by a whole segment. We copy it.
- `RecvSequenceBuffer`: keeps duplicates of delivered packets forever. Stalled
  receives never time out.
- `NGCFT1::iteratePeer`: no INIT retry before the peer's first INIT_ACK (no
  CCA yet).
- ngcext: 0x90 is both HAVE_ALL and the two-byte prefix, old IDs parse first,
  so most two-byte packets read as HAVE_ALL.
