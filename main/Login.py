# Login.py
import streamlit as st
import pandas as pd
import hashlib
import re

USER_DATA = "users.csv"

def create_user_table():
    try:
        pd.DataFrame(columns=['username', 'password']).to_csv(USER_DATA, index=False)
    except:
        pass

def add_user(username, password):
    df = pd.read_csv(USER_DATA)

    # Check if username already exists (case-insensitive)
    if username.lower() in df['username'].str.lower().values:
        return False  # Indicate failure due to existing user

    hashed_pw = hashlib.sha256(password.encode()).hexdigest()
    new_user = pd.DataFrame([[username, hashed_pw]], columns=['username', 'password'])
    df = pd.concat([df, new_user], ignore_index=True)
    df.to_csv(USER_DATA, index=False)
    return True  # Successfully added


def verify_user(username, password):
    df = pd.read_csv(USER_DATA)
    hashed_pw = hashlib.sha256(password.encode()).hexdigest()
    return not df[(df['username'] == username) & (df['password'] == hashed_pw)].empty

def login_page():
    st.markdown("## 🌿 ShadiaBot Authentication")
    
    # Check for success state
    if st.session_state.get("login_success"):
        st.success("✅ Login successful!")
        if st.button("➡️ Go to Chat"):
            # Clear temporary states and set final auth state
            del st.session_state.login_success
            st.session_state.logged_in = True
            st.session_state.current_page = "Main"
            st.rerun()
        return

    tab1, tab2 = st.tabs(["Login", "Register"])
    

    with tab1:
        with st.form("Login"):
            username = st.text_input("Username")
            password = st.text_input("Password", type="password")
            if st.form_submit_button("Sign In"):
                if verify_user(username, password):
                    st.session_state.logged_in = True
                    st.session_state.username = username  # Store username
                    st.session_state.login_success = True
                    st.session_state.current_page = "Main"
                    st.rerun()
                else:
                    st.error("❌ Invalid credentials")

    with tab2:
        with st.form("Register"):
            new_user = st.text_input("New Username")
            new_pw = st.text_input("New Password", type="password")
            
            if st.form_submit_button("Create Account"):
                if len(new_user) > 0 and len(new_pw) > 0:
                    if len(new_pw) < 8:
                        st.warning("⚠️ Password must be at least 8 characters long.")
                    elif not re.search(r'[A-Z]', new_pw):
                        st.warning("⚠️ Password must contain at least one uppercase letter.")
                    elif not re.search(r'\d', new_pw):
                        st.warning("⚠️ Password must contain at least one digit.")
                    else:
                        if add_user(new_user, new_pw):
                            st.success("✅ Account created! Please login.")
                        else:
                            st.error("❌ Username already exists. Please choose another.")
                else:
                    st.warning("⚠️ Please fill all fields.")

    if st.button("← Return to Chat"):
        st.session_state.current_page = "Main"
        st.rerun()
        
if __name__ == "__main__":
    # Only set page config when running Login.py directly
    st.set_page_config(page_title="Login", layout="centered")
    login_page()