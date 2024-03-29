[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/rayatus/sapinterfacemonitor/blob/master/LICENSE)
![ABAP 7.40sp08+](https://img.shields.io/badge/ABAP-7.40sp08+-brightgreen)
[![Code Statistics](https://img.shields.io/badge/CodeStatistics-abaplint-blue)](https://abaplint.app/stats/rayatus/sapinterfacemonitor)

# SAPInterfaceMonitor
SAP interface monitor is a custom framework and application for login communications, rigth now only RFCs (functions) are supported.

![sapinterfacemonitor](https://raw.githubusercontent.com/rayatus/sapinterfacemonitor/master/docs/img/Image%201.png)

# First use
You will find report **ZINTFMONITOR_DUMMY_DATAGEN** that prepares custo for 2 different interfaces (DUMMY01 and DUMMY02) and generates dummy executions so that they could be displayed via transaction **ZINTFMONITOR**.

# How to use it
The main idea is to create an instance via **ZCF_INFTMONITOR=>NEW( )**, add input/output parameters via **LO_INSTANCE->ADD_PARAMETER( )** and before ending save in with **LO_INSTANCE->STORE( )**.

For each interface some customizing has to be done (transaction **ZINTFMONITORC**):
- Ensure that every interface has it's entry in custo.
- Per every interface parameterso that the framework knows how to display them in the monitor application.

In order to display traced executions just execute transaction **ZINTFMONITOR**.

# Disclaimer
SAP Interface Monitor it's mainly developed for learning purposes and has no official support for SAP, so use it at your own risk (nowadays it hasn't been used in productive systems).

