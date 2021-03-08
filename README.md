Huono-osaisuus Suomessa -sovellus
===============================

Sovelluksen pystyttäminen

1. Tallenna tämä lähdekoodi koneellesi ja avaa se uutena RStudion projektina
2. Asenna tarvittavat paketit ajamalla komento: `source("./data_raw/install_packages.R")`
3. Kopioi kansioon `data_raw` seuraavat excel-tiedostot:
    - `Aikasarjadata KORJATTU.xlsx`
    - `Huono-osaisuusindikaattorit - uusimmat.xlsx`
4. Aja komento `source("./data_raw/process_raw_data.R")`
5. Käynnistä sovellus komenolla `shiny::runApp()`

***

(C) 2020-2021 DIAK [MIT](LICENCE)-lisenssi