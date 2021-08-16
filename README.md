# Социальные связи и взгляды в Европе

Этот репозиторий воспроизводит результаты работы "Социальные связи и взгляды в Европе". Все интересующие вопросы можно задать авторам работы: Семен Жижерин (<samzhizherin@gmail.com>), Данила Кочнев (<danila.kochnev2000@mail.ru>).

Наше исследование существенным образом опирается на статью Bailey et al. (2020) [The Determinants of Social Connectedness in Europe](https://doi.org/10.1007/978-3-030-60975-7_1). Мы используем данные, собранные авторами этой статьи и доступные в их [репозитории](https://github.com/social-connectedness-index/euro_sci).

## Структура репозитория

Все результаты работы можно воспроизвести, запустив скрипты из папки `regressions`. Каждый скрипт соответствует одной из рассматриваемых в работе зависимых переменных. В репозитории приведён весь основной код, позволяющий получить промежуточные и финальные наборы данных, однако отсутствуют изначальные, необработанные данные, взятые из открытых источников. Эти данные можно получить при помощи ссылок из файла `data_sources_and_description.xlsx`, который находится в папке `data_description`.
Репозиторий имеет следующую структуру:

* `create_intermediate_data` - скрипты для создания `intermediate_data`
* `create_plots` - скрипты для создания графиков
* `data_description` - описание данных и их источники
* `final_data` - финальные наборы данных для регрессий
* `intermediate_data` - промежуточные наборы данных и пространственные матрицы
* `plots` - графики
* `regressions` - скрипты для регрессионного анализа
* `create_final_data.R` - скрипт для создания `final_data`
* `SC_and_views_in_Europe.pdf` - текст работы
* `Social_connections_and views_in_Europe_ENG.pdf` - английская версия текста работы

Отметим, что в начале каждого скрипта указаны его Inputs и Outputs. В случае, когда в Inputs присутствует какой-либо файл из папок `raw_data` и `borrowed_raw_data`, скрипт не будет корректно работать, если не воспроизвести эти папки и их содержимое при помощи файла `data_sources_and_description.xlsx`. Также мы готовы поделиться этими папками, если интересующиеся свяжутся с нами по электронной почте.

***

# Social connections and views in Europe

This repository reproduces the study "Social Connections and Views in Europe". If you happen to have any questions, please do not hesitate to contact the authors: Semen Zhizherin (<samzhizherin@gmail.com>), Danila Kochnev (<danila.kochnev2000@mail.ru>).

Our research draws heavily on the paper by Bailey et al. (2020) [The Determinants of Social Connectedness in Europe](https://doi.org/10.1007/978-3-030-60975-7_1). We use the data collected by the authors of this article and available in their [repository](https://github.com/social-connectedness-index/euro_sci).

## Repository structure

All results of the study can be reproduced by running scripts from the `regressions` folder. Each script corresponds to one of the dependent variables considered in the work. The repository contains all the main code that allows you to get intermediate and final data sets, but there is no initial, raw data taken from open sources. This data can be obtained using links from the file `data_sources_and_description.xlsx`, which is located in the folder `data_description`.
The repository has the following structure:

* `create_intermediate_data` - scripts that create `intermediate_data`
* `create_plots` - scripts that create plots
* `data_description` - data description and sources
* `final_data` - final regression datasets
* `intermediate_data` - intermediate datasets and spatial matrices
* `plots` - plots
* `regressions` - scripts for regression analysis
* `create_final_data.R` - script that creates `final_data`
* `SC_and_views_in_Europe.pdf` - paper text
* `Social_connections_and views_in_Europe_ENG.pdf` - english version of paper text

Note that at the beginning of each script its Inputs and Outputs are indicated. In the case when any file from the `raw_data` and `borrowed_raw_data` folders is present in Inputs, the script will not work correctly if you do not reproduce these folders and their contents using the `data_sources_and_description.xlsx` file. Also we are ready to share these folders if those interested will contact us via email. 
