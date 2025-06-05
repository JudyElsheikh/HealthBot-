import streamlit as st
from py4j.java_gateway import JavaGateway, GatewayParameters
from PIL import Image
import base64
import time 

# Initialize Py4J Gateway
try:
    gateway = JavaGateway(gateway_parameters=GatewayParameters(auto_convert=True))
    health_bot = gateway.entry_point
except Exception as e:
    st.error(f"❌ Could not connect to Scala backend: {e}")
    st.stop()
# Session state initialization
if 'current_page' not in st.session_state:
    st.session_state.current_page = "Main"

session_states = {
    'messages': [],
    'quiz_state': {'active': False, 'current_question': 0, 'answers': []},
    'symptom_analysis': {'active': False, 'round': 0, 'symptoms': []}
}
for key, default in session_states.items():
    if key not in st.session_state:
        st.session_state[key] = default

# Page config
st.set_page_config(
    page_title="ShadiaBot",
    page_icon="🌿",
    layout="centered",
    initial_sidebar_state="expanded"
)

# Load and encode header image
def get_base64_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode()

header_image = get_base64_image("image.png")
user_image=get_base64_image("user.png")

# Custom CSS with embedded image
st.markdown(f"""
    <style>
        .header-container {{
            display: flex;
            align-items: center;
            justify-content: center;
            gap: 15px;
            margin-bottom: 2rem;
        }}
        .header-title {{
            color: #2d5d2d !important;
            font-size: 2.8em;
            margin: 0;
        }}
        .header-img {{
            height: 50px;
            width: auto;
        }}
    </style>
""", unsafe_allow_html=True)

def sidebar_content():
    with st.sidebar:
        st.markdown("### 🌿 ShadiaBot")

        logged_in = st.session_state.get('logged_in', False)
        current_page = st.session_state.get('current_page', 'Main')

        if logged_in:
            if current_page == "ChatHistory":
                # Just show Return to Chat button, no chat messages
                if st.button("⬅️ Return to Chat", use_container_width=True):
                    st.session_state.messages = []  
                    st.session_state.current_page = "Main"
                    st.rerun()

            else:
                if st.button("🕓 Load Last Session"):
                    try:
                        last_chat = health_bot.getLastChatSession(st.session_state.username)
                        if last_chat:
                            formatted_responses = [
                                line.strip() for line in last_chat if line.strip()
                            ]
                            st.session_state.messages = []  # Clear previous messages
                            for line in formatted_responses:
                                if ':' in line:
                                    speaker, msg = line.split(':', 1)
                                    speaker = speaker.strip().lower()
                                    role = 'assistant' if speaker == 'bot' else 'user'
                                    st.session_state.messages.append({
                                        'role': role,
                                        'content': msg.strip()
                                    })
                            st.session_state.current_page = "ChatHistory"
                            st.rerun()
                        else:
                            st.warning("No previous session found.")
                    except Exception as e:
                        st.error(f"❌ Error loading last session: {e}")

                # User greeting button styled green with white text
                username = st.session_state.get('username', 'Guest')
                st.markdown(
                    f"""
                    <style>
                        .user-badge {{
                            background-color: #2e7d32;
                            color: white;
                            padding: 10px;
                            border-radius: 8px;
                            text-align: center;
                            font-weight: bold;
                            margin-top: 12px;
                        }}
                    </style>
                    <div class="user-badge">👋 Welcome, {username}</div>
                    """,
                    unsafe_allow_html=True
                )

        else:
            if st.button("🔐 Login", key="login_button", use_container_width=True):
                st.session_state.current_page = "Login"
                st.rerun()




def header():
    st.markdown(
        f"""
        <div class="header-container">
            <img src="data:image/png;base64,{header_image}" class="header-img" alt="Bot Icon">
            <h1 class="header-title">ShadiaBot</h1>
        </div>
        """,
        unsafe_allow_html=True
    )

def main_chat_interface():
    with st.container(height=360, border=False) as chat_container:
        for message in st.session_state.messages:
            if message["role"] == "assistant":
                st.markdown(f"""
                    <div style="display: flex; align-items: flex-start; gap: 10px; margin-bottom: 10px;">
                        <img src="data:image/png;base64,{header_image}" style="width: 40px; height: 40px; border-radius: 50%; object-fit: cover;"/>
                        <div style="background-color: #f1f8e9; padding: 10px 15px; border-radius: 10px; color: #2d5d2d; max-width: 80%;">
                            {message["content"]}
                        </div>
                    </div>
                """, unsafe_allow_html=True)
            elif message["role"] == "user":
                st.markdown(f"""
                    <div style="display: flex; align-items: flex-start; gap: 10px; margin-bottom: 10px; flex-direction: row-reverse;">
                        <img src="data:image/png;base64,{user_image}" style="width: 40px; height: 40px; border-radius: 50%; object-fit: cover;"/>
                        <div style="background-color: #e8f5e9; padding: 10px 15px; border-radius: 10px; color: #000000; max-width: 80%;">
                            {message["content"]}
                        </div>
                    </div>
                """, unsafe_allow_html=True)


        st.markdown("<div style='height: 80px'></div>", unsafe_allow_html=True)

    with st.container():
        user_input = st.chat_input("Type your message here...")
        
        st.markdown(
            """
            <div style="margin: 10px 0 20px 0; text-align: center; color: #2d5d2d;
                        font-size: 0.85em; padding: 8px; border-radius: 5px;
                        background-color: #e8f5e9;">
                ⚠️ This chatbot provides general guidance only. Always consult a healthcare professional.
            </div>
            """,
            unsafe_allow_html=True
        )

    if user_input:
        process_user_input(user_input)
        st.rerun()
def process_user_input(user_input: str) -> None:
    st.session_state.messages.append({"role": "user", "content": user_input.strip()})

    try:
        # Send input to Scala
        health_bot.provideInputFromGUI(user_input)

        # Give backend time to process (or use polling with timeout)
        time.sleep(1.0)

        # Get the bot's response
        
        bot_responses = health_bot.getAndClearResponses()
        if bot_responses:
            formatted_responses = [
                line.strip() for line in bot_responses.split('\n') 
                if line.strip()
            ]
            for response in formatted_responses:
                st.session_state.messages.append({
                    "role": "assistant",
                    "content": response
                })
           # st.rerun()

    except Exception as e:
        error_msg = f"❌ Error: {str(e)}"
        st.session_state.messages.append({
            "role": "assistant", 
            "content": error_msg
        })
        st.exception(e)
        st.rerun()



# Update the main app flow section with this code:
if 'logged_in' not in st.session_state:
    st.session_state.logged_in = False


if 'current_page' not in st.session_state:
    st.session_state.current_page = "Main"

# Update the main app flow section with this code:
def main_app_flow():
    sidebar_content()
    header()
  
    # Redirect logic
    if not st.session_state.logged_in and st.session_state.current_page != "Login":
        st.session_state.current_page = "Login"
        st.rerun()
    
    if st.session_state.current_page == "Login":
        from Login import login_page
        login_page()
    else:
        if st.session_state.logged_in:
            if not st.session_state.get('login_count_updated', False):
                try:
                    health_bot.receiveUsername(st.session_state['username'])
                    health_bot.updateLogin(st.session_state['username'])
                    st.session_state.login_count_updated = True  # Mark as updated
                except Exception as e:
                    st.error(f"Could not send username to backend: {e}")
            main_chat_interface()
        else:
            st.warning("Please log in to access the chat")
            if st.button("Return to Login"):
                st.session_state.current_page = "Login"
                st.rerun()

# Run the main app flow
main_app_flow()  