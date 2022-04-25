from __future__ import print_function
import datetime
from airflow import DAG
from airflow.models import Variable
from airflow.operators.dummy_operator import DummyOperator
from airflow.operators.bash_operator import BashOperator
import pendulum

default_args = {                                        ### Default arugments for dag
    'owner': 'airflow',
    'start_date': datetime.datetime(2022, 4, 11),           ###  start date of dag
    'end_date': datetime.datetime(2022, 4, 11)              ###  end date of dag
}

with DAG(
        dag_id='example_variables',        ###  id for the dag
        schedule_interval="@once",        ### schedule on which this dag will run
        start_date=pendulum.datetime(2021, 1, 1, tz="UTC"),   ###  start date for the job
        catchup=False,
        dagrun_timeout=datetime.timedelta(minutes=60),  ### timeout interval
        tags=['example', 'example2'],    ### tags for filtering
        params={"example_key": "example_value"}, ###  parameters (optional)
) as dag:
    firstname_lowercase = BashOperator(    ###  bash operator uses linux terminal to run commands
        task_id='task1.1', ###  task id
        do_xcom_push=True, ###  data communication using x com setting it to true so all the output is stored in x-com
        bash_command='echo "spyros"' ###  running the echo command and value will be pushed in x com
    )

    lastname_lowercase = BashOperator(
        task_id='task1.2',
        do_xcom_push=True,  ###  data communication using x com setting it to true so all the output is stored in x-com
        bash_command='echo "despotis"' ###  running the echo command and value will be pushed in x com
    )

    firstname_uppercase = BashOperator(
        task_id='task2.1',
        do_xcom_push=True,
        bash_command=' captial={{ ti.xcom_pull(task_ids="task1.1")}};echo ${captial^}' ###  taking data from xcom variable and putting it into a shell variable then using the uppercase operator to make first letter captial and value will be pushed in x com
    )

    lastname_uppercase = BashOperator(
        task_id='task2.2',
        do_xcom_push=True,
        bash_command=' captial={{ ti.xcom_pull(task_ids="task1.2")}};echo ${captial^}' ###  taking data from xcom variable and putting it into a shell variable then using the uppercase operator to make first letter captial and value will be pushed in x com
    )
    fullname = BashOperator(
        task_id='task3',
        do_xcom_push=True,
        bash_command=' first={{ ti.xcom_pull(task_ids="task2.1")}};last={{ ti.xcom_pull(task_ids="task2.2")}};echo "${first} ${last}"' ## taking values of first name and lastname from both tasks and then combining them in echo.
    )

    firstname_lowercase >> firstname_uppercase >> fullname  ### adding dependency and order of running these tasks
    lastname_lowercase >> lastname_uppercase >> fullname
