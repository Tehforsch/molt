fn foo() {
    loop {
        break;
        break 'a;
        continue;
    }

    'label: for foo in 0..100 {
    }

    'a: loop {
    }
}
