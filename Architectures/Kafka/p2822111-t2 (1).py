from datetime import datetime
import random
import uuid
import json
from kafka import KafkaProducer
from kafka import KafkaConsumer

KAFKA_TOPIC = 'clima'  # topic name for kafka topic where we will send data.


class Data:  # class to store data
    def __init__(self):  # init method
        self.id = str(uuid.uuid4())  # generating random ID to asign for each new data object
        self.temperature = random.randint(0, 50)  # Initialzing random value for temperature between 0 and 50
        self.humidity = random.randint(0, 10)  # Initialzing random value for humidity between 0 and 100
        self.timestamp = str(datetime.now())  # Gettting current timestamp (date time)

    def to_json(self):
        return json.dumps(self.__dict__)


def send_message_to_kafka(data):
    producer = KafkaProducer(bootstrap_servers='127.0.0.1:9092')  # Initializing Producer to send data to kafka . using local kafka server on port 9092 and topic is clima
    producer.send(topic=KAFKA_TOPIC, key=data.id.encode(), value=json.dumps(data.to_json()).encode())  # using send method to send data to kafka. encoding json to byte (requirement by kafka)
    producer.flush()


if __name__ == '__main__':

    print("Start to send messages to kafka using Send method...")
    for i in range(0, 5):  # Looping on the range of 5 to send 5 messages
        data = Data()  # Generating random data (temp + humidity + datetime) for each iteration
        send_message_to_kafka(data)  # Sending data to kafka
        print(f'Sending message {data.to_json()} to kafka')  # printing for visibility
    print("Start to receive messages from kafka...")
    consumer = KafkaConsumer(KAFKA_TOPIC, bootstrap_servers="localhost:9092", auto_offset_reset='earliest')  # Initializing consumer for kafka to receive data. using local kafka server on port 9092 and topic is clima
    for record in consumer:  # Taking/receiving one message at a time from consumer
        print(f"Received from Kafka = {json.loads(record.value.decode())}")  # printing the message by dcoding bytes to string
