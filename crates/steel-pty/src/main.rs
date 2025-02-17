//! This is a conceptually simple example that spawns the `whoami` program
//! to print your username.  It is made more complex because there are multiple
//! pipes involved and it is easy to get blocked/deadlocked if care and attention
//! is not paid to those pipes!
use portable_pty::{CommandBuilder, NativePtySystem, PtySize, PtySystem};
use std::{io::Read, sync::mpsc::channel};

fn main() {
    let pty_system = NativePtySystem::default();

    let pair = pty_system
        .openpty(PtySize {
            rows: 24,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .unwrap();

    let cmd = CommandBuilder::new("/bin/bash");
    let mut child = pair.slave.spawn_command(cmd).unwrap();

    // Release any handles owned by the slave: we don't need it now
    // that we've spawned the child.
    drop(pair.slave);

    // Read the output in another thread.
    // This is important because it is easy to encounter a situation
    // where read/write buffers fill and block either your process
    // or the spawned process.
    let (tx, rx) = channel();

    let (cancellation_token_sender, cancellation_token_receiver) = channel::<()>();

    // let stay_alive = tx.clone();

    let mut reader = pair.master.try_clone_reader().unwrap();
    let mut writer = pair.master.take_writer().unwrap();

    std::thread::spawn(move || {
        // Consume the output from the child

        // let mut bufreader = BufReader::new(reader);

        let mut read_buffer = [0; 65536];

        loop {
            // let mut read_buffer = [0; 65536];

            // let mut s = String::new();
            // bufreader.read_line(&mut s).unwrap();

            let size = reader.read(&mut read_buffer).unwrap();

            // reader.read_to_string(&mut s).unwrap();

            if size != 0 {
                let s = String::from_utf8_lossy(&read_buffer[..size]).to_string();

                let r = tx.send(s);

                r.unwrap();
            }

            if cancellation_token_receiver.try_recv().is_ok() {
                break;
            }
        }
    });

    let (command_sender, command_receiver) = channel::<String>();

    {
        // Obtain the writer.
        // When the writer is dropped, EOF will be sent to
        // the program that was spawned.
        // It is important to take the writer even if you don't
        // send anything to its stdin so that EOF can be
        // generated, otherwise you risk deadlocking yourself.
        // let mut writer = pair.master.take_writer().unwrap();

        if cfg!(target_os = "macos") {
            // macOS quirk: the child and reader must be started and
            // allowed a brief grace period to run before we allow
            // the writer to drop. Otherwise, the data we send to
            // the kernel to trigger EOF is interleaved with the
            // data read by the reader! WTF!?
            // This appears to be a race condition for very short
            // lived processes on macOS.
            // I'd love to find a more deterministic solution to
            // this than sleeping.
            std::thread::sleep(std::time::Duration::from_millis(20));
        }

        // This example doesn't need to write anything, but if you
        // want to send data to the child, you'd set `to_write` to
        // that data and do it like this:
        // let to_write = "ls -l";
        // if !to_write.is_empty() {
        // To avoid deadlock, wrt. reading and waiting, we send
        // data to the stdin of the child in a different thread.
        std::thread::spawn(move || loop {
            while let Ok(command) = command_receiver.recv() {
                writer.write_all(command.as_bytes()).unwrap();
            }
        });
        // }
    }

    command_sender.send("ls -l\r".to_string()).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(20));

    command_sender.send("ls -l\r".to_string()).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(20));
    // Wait for the child to complete
    // println!("child status: {:?}", child.wait().unwrap());

    child.kill().unwrap();
    cancellation_token_sender.send(()).unwrap();

    // child.wa

    // Take care to drop the master after our processes are
    // done, as some platforms get unhappy if it is dropped
    // sooner than that.
    drop(pair.master);

    // Now wait for the output to be read by our reader thread
    // let output = rx.recv().unwrap();

    for line in rx {
        print!("{}", line);
    }

    // We print with escapes escaped because the windows conpty
    // implementation synthesizes title change escape sequences
    // in the output stream and it can be confusing to see those
    // printed out raw in another terminal.
    // print!("output: ");
    // TODO: Include this back when rendering to the built in terminal
    // for c in output.escape_debug() {
    //     print!("{}", c);
    // }

    // print!("{}", output);

    // for c in output {
    // print!("{}", c);
    // }
}
