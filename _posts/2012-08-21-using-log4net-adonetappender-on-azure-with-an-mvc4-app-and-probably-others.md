---
title: "Using log4net ADONetAppender on Azure with an MVC4 app (and probably others)"
date: "2012-08-21"
categories: 
  - "Programming#Misc"
---

There are three setup steps:

1. Get log4net
2. Create a table on Azure
3. Update your web.config

Once these are done you can use your logger.

# Get log4net

Use NuGet to get Log4Net.

![image](/images/archive/image_thumb.webp "image")

# Create a table on Azure

I created a table using the database management website – the database looks like this:

![image](/images/archive/image_thumb1.webp "image")

It’s really important that the ID column be an Identity column – Azure DB requires clustered indexes.

# Update your web.config

Add these to your web.config or app.config

Add the config section entry:

```xml
<section name="log4net" type="log4net.Config.Log4NetConfigurationSectionHandler, log4net" />
```

And then this section

```xml
<log4net>  
 <appender name="ADONetAppender" type="log4net.Appender.ADONetAppender">  
   <bufferSize value="1" />  
   <connectionType value="System.Data.SqlClient.SqlConnection, System.Data, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" />  
   <connectionStringName value="DefaultConnection" />  
   <commandText value="INSERT INTO Log ([Date],[Level],[Message],[Exception]) VALUES (@log_date, @log_level, @message, @exception)" />  
   <parameter>  
     <parameterName value="@log_date"/>  
     <dbType value="DateTime"/>  
     <layout type="log4net.Layout.RawTimeStampLayout"/>  
   </parameter>  
   <parameter>  
     <parameterName value="@log_level"/>  
     <dbType value="String"/>  
     <size value="50"/>  
     <layout type="log4net.Layout.PatternLayout">  
     <conversionPattern value="%level"/>  
     </layout>  
   </parameter>  
   <parameter>  
     <parameterName value="@message"/>  
     <dbType value="String"/>  
     <size value="4000"/>  
     <layout type="log4net.Layout.PatternLayout">  
     <conversionPattern value="%message"/>  
     </layout>  
   </parameter>  
   <parameter>  
     <parameterName value="@exception"/>  
     <dbType value="String"/>  
     <size value="4000"/>  
     <layout type="log4net.Layout.ExceptionLayout"/>  
   </parameter>  
 </appender>

 <root>  
  <level value="All"/>  
  <appender-ref ref="ADONetAppender"/>  
 </root>  
</log4net>
```

Notice that the buffer size is set to 1 – this will flush after every log. Maybe change that to something larger when you go to scale.

# Using the Logger

First initialize the logger. You can add this to whatever startup code you want (Global.asax.cs, OnStart, etc) …

```csharp
log4net.Config.XmlConfigurator.Configure();
```

You can now add a logger to your code like this:

```csharp
private static readonly log4net.ILog log = log4net.LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().DeclaringType);
```

And use it like this:

```csharp
log.Debug("message…");
```