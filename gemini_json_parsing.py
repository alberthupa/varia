# Copyright 2024 Google LLC

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#     https://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import streamlit as st

#### GenAI section ########
# @title Specify poject and region settings
import uuid
import vertexai
import mimetypes, time
from google.cloud import storage
from vertexai.generative_models import (
    GenerationConfig,
    GenerativeModel,
    Part,
)

PROJECT_ID = "" 
REGION = ""  
bucket_name = ""
MODEL_ID = "gemini-1.5-flash-001"  

@st.cache_resource
def setup_models():
  vertexai.init(project=PROJECT_ID, location=REGION)

  model_flow_selection = GenerativeModel(
      MODEL_ID,
      system_instruction=[
          "You are a semantic router.",
          "Your mission is to always match the PROMPT to a specific flow.",
          "The flows are as follows:",
          "\n- If the PROMPT is about being sick, output FLOW-1",
          "\n- If the PROMPT is about a machine not working, output FLOW-2",
          "\n- If the PROMPT did not match one of the above and is a general greeting or blank, output FLOW-GREETING",
          "\n- If the PROMPT did not match one of the above and is a thank you or good bye, output FLOW-DONE"
      ],
  )

  model_flow_1_sick = GenerativeModel(
      MODEL_ID,
      system_instruction=[
          "You are an assistant to the Store Manager. Please follow the next guideline-context to answer the employee. Be agreeable and helpful.",
          "Always read and think about CHAT_HISTORY before responding to the customer.",
          "guideline-context:",
          "\n- If the employee says they are sick, respond with concern and ask how long they expect to be out.",
          "\n- If the employee says how long they are going to be out sick, inform them that you are contacting the Store Manager, locating shift replacements, and will followup in (one day less than their days sick) time to check up on them.",
          "\n- If the customer says thank you, bye, or similar - respond in kind. At the end of your message output <FLOW-DONE>.",
      ],
  )

  model_flow_2_bad_machine = GenerativeModel(
      MODEL_ID,
      system_instruction=[
          "You are a helpful technician of the  technical team. Please follow the next guideline-context to answer the employee. Be agreeable and helpful.",
          "Always read and think about CHAT_HISTORY before responding to the customer.",
          "guideline-context:",
          "\n- If the employee asks a technical question about a machine and there is no video, ask for a video of it",
          "\n- If the employee sends an video or diagram: analyze the video and give a step by step procedure in bullet points of what needs to be done to fix the machine. At the end - ask them if that worked or if they need more help.",
          "\n- If the customer says thank you, bye, or similar - respond in kind. At the end of your message output <FLOW-DONE>.",
      ],
  )


  return model_flow_selection, model_flow_1_sick, model_flow_2_bad_machine


@st.cache_resource
def get_storage_client():
  return storage.Client()

@st.cache_resource
def get_bucket_from_storage_client():
  return get_storage_client().bucket(bucket_name)

def list_blobs(bucket_name):
    """Lists all the blobs in the bucket."""
    storage_client = get_storage_client()
    blobs = storage_client.list_blobs(bucket_name)

    for blob in blobs:
        print(f"gs://{bucket_name}/{blob.name}")

def upload_file(bucket_name, source_file_name, destination_blob_name):
    storage_client = get_storage_client()
    bucket = storage_client.bucket(bucket_name)
    blob = bucket.blob(destination_blob_name)
    generation_match_precondition = 0
    blob.upload_from_filename(source_file_name, num_retries=2)
    print(f"File {source_file_name} uploaded to {destination_blob_name}.")
    #This could be used to go straight from the base64 string to GCS
    # bucket = storage.bucket()
    # blob = bucket.blob(path)
    # regex = r"(?<=data:)(.*)(?=;)"
    # split = image.split('base64')
    # format_image = re.findall(regex, split[0])[0]
    # base64_image = base64.b64decode(split[1])
    # blob.upload_from_string(base64_image, content_type=format_image)


def flow_manager(model_flow: GenerativeModel, currentState: str, gcsFileLink: str, chatHistory: str, user_prompt: str) -> tuple[str, str, str, str]:
  #execute the incoming user_prompt
  # rely on the prompt to understand the current state passed into it
  user_prompt_with_history = f"<CHAT_HISTORY>{chatHistory}\n</CHAT_HISTORY>\nNew Prompt: {user_prompt}"

  #check if gcsFileLink has something
  # if it does - guess the mime type and attach it to the call
  content=None
  if gcsFileLink != "":
    mime_type = mimetypes.guess_type(gcsFileLink)[0]
    print(f"Mime-type guessed: {mime_type}")
    content_file1 = Part.from_uri(gcsFileLink,mime_type=mime_type)
    content = [user_prompt_with_history, content_file1]
    gcsFileLink = ""
  else:
    content = user_prompt_with_history

  print(content)
  for n in range(4):
    try:
        generation_config = GenerationConfig(
                                temperature=0.5,
                                top_p=1.0,
                                top_k=32,
                                candidate_count=1,
                                max_output_tokens=1000,
                                #response_mime_type="application/json",
                            )
        response = model_flow.generate_content(content, generation_config=generation_config)
    except Exception as e:
        print(e)
        time.sleep(5)
        continue
    else:
        break
  if str(response.candidates[0].finish_reason) == "FinishReason.SAFETY":
    prompt_output = ""
  else:
    prompt_output = response.text.replace("\n","").strip()
  if prompt_output.find("<FLOW-DONE>") != -1:
    currentState = ""
    chatHistory = ""
    print("Reached FLOW-DONE state!")
    prompt_output = prompt_output.replace("<FLOW-DONE>","")
  else:
    chatHistory = chatHistory + "\nUser: " + user_prompt
    chatHistory = chatHistory + "\nSystem: " + prompt_output
  return currentState, gcsFileLink, chatHistory, prompt_output

def process_utterance(currentState: str, gcsFileLink: str, chatHistory: str, user_prompt: str) -> tuple[str, str, str, str]:
  #if we have no state or if a user
  # has finished their workflow
  # via a thank you or all done
  # and the workflow reset state,
  # check user_prompt for new workflow
  prompt_output = ""
  #load the models
  model_flow_selection, model_flow_1_sick, model_flow_2_bad_machine = setup_models()

  if currentState == "":
    for n in range(4):
        try:
            response = model_flow_selection.generate_content(user_prompt)
        except Error as e:
            print(e)
            time.sleep(5)
            continue
        else:
            break
    
    if str(response.candidates[0].finish_reason) == "FinishReason.SAFETY":
      flow_selection = ""
    else:
      flow_selection = response.text.replace("\n","").strip()
      chatHistory = chatHistory + "\nUser: " + user_prompt
      chatHistory = chatHistory + "\nSystem: Directed user to workflow " + flow_selection
    print(f"flow_selection: '{flow_selection}'")
    match flow_selection:
      case "FLOW-1" | "FLOW-2" :
        currentState = flow_selection
      case "FLOW-GREETING":
        prompt_output = "Hi human! What can I help you with?"
        currentState = ""
      case "FLOW-DONE":
        prompt_output = "Bye human! Let me know if I can help further!"
        currentState = flow_selection
      case _:
        #catch all for no hit or
        # safety violation - dont store results
        prompt_output = "Sorry, I didn't get that."
  match currentState:
    case "FLOW-1":
      #pass state, link, history, user prompt into flow 1
      print("Calling FLOW-1")
      currentState, gcsFileLink, chatHistory, prompt_output = flow_manager(model_flow_1_sick, currentState, gcsFileLink, chatHistory, user_prompt)
    case "FLOW-2":
      print("Calling FLOW-2")
      currentState, gcsFileLink, chatHistory, prompt_output = flow_manager(model_flow_2_bad_machine, currentState, gcsFileLink, chatHistory, user_prompt)
    case _:
      # reset current state - something went wrong
      currentState = ""
  return currentState, gcsFileLink, chatHistory, prompt_output

 ##############################################################################

st.set_page_config(
page_title="Shift Assistant Manager",
page_icon="ice_cube:",
layout="centered",
initial_sidebar_state="collapsed",
)
st.title("Shift Assistant Manager")
st.sidebar.success("Select a page above.")

css = '''
<style>
    [data-testid="stChatMessage"] {
        padding: 0px;
    }
    [data-testid='stFileUploader'] {
        padding: 5px;
    }
    [data-testid='stFileUploader'] section {
        padding: 5px;
    }
    [data-testid='stFileUploader'] section > input + div {
        padding: 5px;
    }
    [data-testid='stFileUploader'] section + div {
        padding: 5px;
    }
    /*[data-testid="stForm"] {
      position: fixed;
      width: calc(100% - 60%);
      bottom: 3rem;
    }*/
   div:has(> [data-testid="stButton"]) {
      position: fixed;
      bottom: 9px;
      z-index: 10000;
    }
</style>
'''
st.markdown(css, unsafe_allow_html=True)

# seed msg, init chat history
if "messages" not in st.session_state:
    st.session_state.messages = [
        {
            "role":"assistant",
            "content":"Ask Assistant to the Store Manager anything || Upload File via :paperclip:"
        }
    ]

@st.experimental_dialog("Upload...")
def show_file_upload():
  uploaded_file = st.file_uploader("Upload an image, video, or pdf", type=["pdf", "jpg", "png", "jpeg", "mp4"], label_visibility="collapsed")
  if uploaded_file:
    # Store file in GCS and get URI
    print(f"Uploading file: {uploaded_file.name}")
    bucket = get_bucket_from_storage_client()
    blob = bucket.blob(f"{str(uuid.uuid4())[:4]}-{uploaded_file.name}")
    blob.upload_from_file(uploaded_file)
    st.session_state.gcsFileLink = f"gs://{bucket_name}/{blob.name}"
    #close the modal dialog now that file is uploaded and URI stored in the session state
    st.rerun()
  return uploaded_file


instr = "Type a message..."
user_input = st.chat_input(instr)
uploaded_file = None
if st.button(":paperclip:"):
  uploaded_file = show_file_upload()

# Chat history display
for message in st.session_state.messages:
    avatar=":material/support_agent:"
    if message["role"] == "assistant":
      avatar=":material/support_agent:"
    else:
      avatar = "🗨️"
    with st.chat_message(message["role"], avatar=avatar):
      st.write(message["content"])

if 'currentState' not in st.session_state:
    st.session_state.currentState = ""
    st.session_state.gcsFileLink = ""
    st.session_state.chatHistory = ""
    st.session_state.prompt_output = ""
##############
if uploaded_file and not user_input:
  st.warning("Please enter a message along with your file.")  # Warning if text is missing
elif user_input:  # Check both submit and user input
    print(f"Current State: {st.session_state.currentState}")
    print(f"GCS File Link: {st.session_state.gcsFileLink}")
    st.session_state.messages.append(
      {
        "role":"user",
        "content": user_input
      }
    )
    with st.chat_message("user", avatar="🗨️"):
        st.write(user_input)
    with st.spinner('Thinking...'):
      st.session_state.currentState, st.session_state.gcsFileLink, st.session_state.chatHistory, st.session_state.prompt_output = process_utterance(
                                                            st.session_state.currentState,
                                                            st.session_state.gcsFileLink,
                                                            st.session_state.chatHistory,
                                                            user_input)
      st.session_state.gcsFileLink = ""
    # display chat msgs from history upon rerun
    st.session_state.messages.append(
        {
          "role":"assistant",
          "content": st.session_state.prompt_output
        }
    )
    with st.chat_message("assistant", avatar=":material/support_agent:"):
        st.markdown(st.session_state.prompt_output)
