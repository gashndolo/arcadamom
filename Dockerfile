FROM openjdk:8-alpine

COPY target/uberjar/arcadamom.jar /arcadamom/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/arcadamom/app.jar"]
